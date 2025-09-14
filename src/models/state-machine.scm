;;; State Machine Module for Change Request Lifecycle
;;; ITIL-compliant state transitions with validation

(define-module (models state-machine)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 format)
  #:use-module (db connection)
  #:export (valid-transition?
            transition-state!
            get-next-states
            get-previous-states
            can-transition?
            record-transition!
            get-transition-history
            validate-transition-requirements
            get-state-metadata
            is-terminal-state?
            is-initial-state?
            get-all-states
            get-state-transitions-map))

;; ITIL-compliant state transition map
(define state-transitions
  '((submitted . (assessing cancelled))
    (assessing . (approved rejected needs-info cancelled))
    (needs-info . (assessing cancelled))
    (approved . (implementing cancelled))
    (implementing . (completed failed cancelled))
    (rejected . ())
    (cancelled . ())
    (completed . ())
    (failed . (implementing cancelled))))

;; State metadata including requirements and constraints
(define state-metadata
  '((submitted . ((description . "Initial state when change request is created")
                  (requires . ())
                  (terminal . #f)))
    (assessing . ((description . "Change is being evaluated for risk and impact")
                  (requires . (risk-assessment))
                  (terminal . #f)))
    (needs-info . ((description . "Additional information required")
                   (requires . ())
                   (terminal . #f)))
    (approved . ((description . "Change has been approved by CAB")
                 (requires . (approval))
                 (terminal . #f)))
    (implementing . ((description . "Change is being implemented")
                     (requires . (implementation-plan))
                     (terminal . #f)))
    (completed . ((description . "Change successfully implemented")
                  (requires . (test-results))
                  (terminal . #t)))
    (failed . ((description . "Change implementation failed")
               (requires . (failure-report))
               (terminal . #f)))
    (rejected . ((description . "Change was rejected")
                 (requires . (rejection-reason))
                 (terminal . #t)))
    (cancelled . ((description . "Change was cancelled")
                  (requires . (cancellation-reason))
                  (terminal . #t)))))

(define (get-all-states)
  "Get list of all valid states"
  (map car state-transitions))

(define (get-state-transitions-map)
  "Get the complete state transitions map"
  state-transitions)

(define (valid-transition? from-state to-state)
  "Check if a transition from one state to another is valid"
  (let ((allowed-transitions (assoc-ref state-transitions from-state)))
    (and allowed-transitions
         (member to-state allowed-transitions))))

(define (get-next-states current-state)
  "Get list of valid next states from current state"
  (or (assoc-ref state-transitions current-state) '()))

(define (get-previous-states target-state)
  "Get list of states that can transition to target state"
  (filter-map
   (lambda (transition)
     (if (member target-state (cdr transition))
         (car transition)
         #f))
   state-transitions))

(define (is-terminal-state? state)
  "Check if a state is terminal (no further transitions)"
  (let ((metadata (assoc-ref state-metadata state)))
    (and metadata
         (assoc-ref metadata 'terminal))))

(define (is-initial-state? state)
  "Check if a state is the initial state"
  (eq? state 'submitted))

(define (get-state-metadata state)
  "Get metadata for a specific state"
  (assoc-ref state-metadata state))

(define (can-transition? change-request-id from-state to-state)
  "Check if a specific change request can transition between states"
  (with-db-connection
   (lambda (conn)
     ;; Check basic transition validity
     (unless (valid-transition? from-state to-state)
       (error "Invalid state transition" from-state to-state))

     ;; Check for any blockers
     (let ((blockers (execute-query conn
                      "SELECT COUNT(*) FROM dependencies
                       WHERE change_request_id = ?
                       AND dependency_type = 'blocks'
                       AND depends_on_id IN (
                         SELECT id FROM change_requests
                         WHERE status NOT IN ('completed', 'cancelled', 'rejected')
                       )" change-request-id)))
       (zero? (caar blockers))))))

(define (validate-transition-requirements change-request-id to-state)
  "Validate that all requirements for transitioning to a state are met"
  (let ((metadata (get-state-metadata to-state)))
    (if metadata
        (let ((requirements (assoc-ref metadata 'requires)))
          (with-db-connection
           (lambda (conn)
             ;; Check specific requirements based on state
             (cond
              ((member 'risk-assessment requirements)
               (let ((assessments (execute-query conn
                                   "SELECT COUNT(*) FROM risk_assessments
                                    WHERE change_request_id = ?" change-request-id)))
                 (unless (> (caar assessments) 0)
                   (error "Risk assessment required for transition to" to-state))))

              ((member 'approval requirements)
               (let ((approvals (execute-query conn
                                 "SELECT COUNT(*) FROM approvals
                                  WHERE change_request_id = ?
                                  AND decision = 'approved'" change-request-id)))
                 (unless (> (caar approvals) 0)
                   (error "Approval required for transition to" to-state))))

              ((member 'implementation-plan requirements)
               (let ((plan (execute-query conn
                            "SELECT implementation_plan FROM change_requests
                             WHERE id = ?" change-request-id)))
                 (when (or (null? plan) (null? (caar plan)))
                   (error "Implementation plan required for transition to" to-state))))

              (else #t)))))
        #t)))

(define (transition-state! change-request-id from-state to-state user-id reason)
  "Perform a state transition for a change request"
  (unless (valid-transition? from-state to-state)
    (error "Invalid state transition" from-state to-state))

  ;; Validate requirements
  (validate-transition-requirements change-request-id to-state)

  (with-db-connection
   (lambda (conn)
     (with-transaction conn
      (lambda (conn)
        ;; Update the change request status
        (let ((updated (execute-update conn
                        "UPDATE change_requests SET status = ?, updated_at = CURRENT_TIMESTAMP
                         WHERE id = ? AND status = ?"
                        to-state change-request-id from-state)))

          (unless (> updated 0)
            (error "Failed to update change request status" change-request-id))

          ;; Record the transition
          (execute-insert conn
           "INSERT INTO state_transitions (change_request_id, from_state, to_state, transitioned_by, reason)
            VALUES (?, ?, ?, ?, ?)"
           change-request-id from-state to-state user-id reason)

          ;; Create notification for state change
          (execute-insert conn
           "INSERT INTO notifications (user_id, change_request_id, notification_type, title, message)
            SELECT requestor_id, ?, 'state_change',
                   'Change Request Status Updated',
                   'Your change request ' || ? || ' has transitioned from ' || ? || ' to ' || ?
            FROM change_requests WHERE id = ?"
           change-request-id change-request-id from-state to-state change-request-id)

          ;; If transitioning to 'approved', notify implementer
          (when (eq? to-state 'approved)
            (execute-insert conn
             "INSERT INTO notifications (user_id, change_request_id, notification_type, title, message)
              SELECT implementer_id, ?, 'state_change',
                     'Change Request Ready for Implementation',
                     'Change request ' || ? || ' has been approved and is ready for implementation'
              FROM change_requests WHERE id = ? AND implementer_id IS NOT NULL"
             change-request-id change-request-id change-request-id))

          to-state))))))

(define (record-transition! change-request-id from-state to-state user-id reason)
  "Record a state transition in the audit log"
  (with-db-connection
   (lambda (conn)
     (execute-insert conn
      "INSERT INTO state_transitions (change_request_id, from_state, to_state, transitioned_by, reason)
       VALUES (?, ?, ?, ?, ?)"
      change-request-id
      (if from-state (symbol->string from-state) #f)
      (symbol->string to-state)
      user-id
      reason))))

(define (get-transition-history change-request-id)
  "Get the complete transition history for a change request"
  (with-db-connection
   (lambda (conn)
     (let ((results (execute-query conn
                     "SELECT st.*, u.full_name as transitioned_by_name
                      FROM state_transitions st
                      LEFT JOIN users u ON st.transitioned_by = u.id
                      WHERE st.change_request_id = ?
                      ORDER BY st.transition_time ASC"
                     change-request-id)))

       (map (lambda (row)
              `((from_state . ,(list-ref row 2))
                (to_state . ,(list-ref row 3))
                (transitioned_by . ,(list-ref row 4))
                (transitioned_by_name . ,(list-ref row 7))
                (reason . ,(list-ref row 5))
                (transition_time . ,(list-ref row 6))))
            results)))))