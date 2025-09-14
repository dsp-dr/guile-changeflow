;;; Change Request Model with Full CRUD Operations
;;; ITIL-compliant change management record type

(define-module (models change-request)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (db connection)
  #:export (make-change-request
            change-request?
            change-request-id
            change-request-title
            change-request-description
            change-request-risk-score
            change-request-status
            change-request-priority
            change-request-category
            change-request-impact
            change-request-urgency
            change-request-requestor-id
            change-request-assignee-id
            change-request-approver-id
            change-request-implementer-id
            change-request-created-at
            change-request-updated-at
            change-request-scheduled-start
            change-request-scheduled-end

            ;; CRUD operations
            create-change-request!
            get-change-request
            update-change-request!
            delete-change-request!
            list-change-requests
            search-change-requests

            ;; Utility functions
            generate-change-id
            validate-change-request
            change-request->alist
            alist->change-request))

;; Define the change request record type
(define-record-type <change-request>
  (make-change-request id title description risk-score status priority category
                       impact urgency requestor-id assignee-id approver-id
                       implementer-id created-at updated-at scheduled-start scheduled-end
                       business-justification implementation-plan backout-plan
                       test-plan communication-plan external-id source-system tags metadata)
  change-request?
  (id change-request-id)
  (title change-request-title)
  (description change-request-description)
  (risk-score change-request-risk-score)
  (status change-request-status)
  (priority change-request-priority)
  (category change-request-category)
  (impact change-request-impact)
  (urgency change-request-urgency)
  (requestor-id change-request-requestor-id)
  (assignee-id change-request-assignee-id)
  (approver-id change-request-approver-id)
  (implementer-id change-request-implementer-id)
  (created-at change-request-created-at)
  (updated-at change-request-updated-at)
  (scheduled-start change-request-scheduled-start)
  (scheduled-end change-request-scheduled-end)
  (business-justification change-request-business-justification)
  (implementation-plan change-request-implementation-plan)
  (backout-plan change-request-backout-plan)
  (test-plan change-request-test-plan)
  (communication-plan change-request-communication-plan)
  (external-id change-request-external-id)
  (source-system change-request-source-system)
  (tags change-request-tags)
  (metadata change-request-metadata))

(define (generate-change-id)
  "Generate a unique change request ID"
  (format #f "CHG-~a-~4,'0d"
          (date->string (current-date) "~Y~m~d")
          (random 10000)))

(define (validate-change-request cr)
  "Validate a change request has required fields"
  (and (change-request? cr)
       (string? (change-request-title cr))
       (not (string-null? (change-request-title cr)))
       (string? (change-request-description cr))
       (not (string-null? (change-request-description cr)))
       (string? (change-request-requestor-id cr))
       (member (change-request-status cr)
               '("submitted" "assessing" "approved" "rejected"
                 "implementing" "completed" "failed" "cancelled" "needs-info"))
       (member (change-request-priority cr)
               '("low" "medium" "high" "critical"))
       (<= 0 (change-request-risk-score cr) 100)))

(define (change-request->alist cr)
  "Convert change request to association list"
  `((id . ,(change-request-id cr))
    (title . ,(change-request-title cr))
    (description . ,(change-request-description cr))
    (risk_score . ,(change-request-risk-score cr))
    (status . ,(change-request-status cr))
    (priority . ,(change-request-priority cr))
    (category . ,(change-request-category cr))
    (impact . ,(change-request-impact cr))
    (urgency . ,(change-request-urgency cr))
    (requestor_id . ,(change-request-requestor-id cr))
    (assignee_id . ,(change-request-assignee-id cr))
    (approver_id . ,(change-request-approver-id cr))
    (implementer_id . ,(change-request-implementer-id cr))
    (created_at . ,(if (date? (change-request-created-at cr))
                        (date->string (change-request-created-at cr) "~Y-~m-~d ~H:~M:~S")
                        (change-request-created-at cr)))
    (updated_at . ,(if (date? (change-request-updated-at cr))
                        (date->string (change-request-updated-at cr) "~Y-~m-~d ~H:~M:~S")
                        (change-request-updated-at cr)))))

(define (alist->change-request alist)
  "Convert association list to change request"
  (make-change-request
   (assoc-ref alist 'id)
   (assoc-ref alist 'title)
   (assoc-ref alist 'description)
   (or (assoc-ref alist 'risk_score) 0)
   (or (assoc-ref alist 'status) "submitted")
   (or (assoc-ref alist 'priority) "medium")
   (or (assoc-ref alist 'category) "standard")
   (or (assoc-ref alist 'impact) "low")
   (or (assoc-ref alist 'urgency) "low")
   (assoc-ref alist 'requestor_id)
   (assoc-ref alist 'assignee_id)
   (assoc-ref alist 'approver_id)
   (assoc-ref alist 'implementer_id)
   (or (assoc-ref alist 'created_at) (current-date))
   (or (assoc-ref alist 'updated_at) (current-date))
   (assoc-ref alist 'scheduled_start)
   (assoc-ref alist 'scheduled_end)
   (assoc-ref alist 'business_justification)
   (assoc-ref alist 'implementation_plan)
   (assoc-ref alist 'backout_plan)
   (assoc-ref alist 'test_plan)
   (assoc-ref alist 'communication_plan)
   (assoc-ref alist 'external_id)
   (assoc-ref alist 'source_system)
   (assoc-ref alist 'tags)
   (assoc-ref alist 'metadata)))

;; CRUD Operations

(define (create-change-request! cr)
  "Create a new change request in the database"
  (unless (validate-change-request cr)
    (error "Invalid change request" cr))

  (with-db-connection
   (lambda (conn)
     (with-transaction conn
      (lambda (conn)
        (execute-insert conn
         "INSERT INTO change_requests (
            id, title, description, risk_score, status, priority, category,
            impact, urgency, requestor_id, assignee_id, approver_id, implementer_id,
            business_justification, implementation_plan, backout_plan,
            test_plan, communication_plan, external_id, source_system, tags, metadata
          ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
         (change-request-id cr)
         (change-request-title cr)
         (change-request-description cr)
         (change-request-risk-score cr)
         (change-request-status cr)
         (change-request-priority cr)
         (change-request-category cr)
         (change-request-impact cr)
         (change-request-urgency cr)
         (change-request-requestor-id cr)
         (change-request-assignee-id cr)
         (change-request-approver-id cr)
         (change-request-implementer-id cr)
         (change-request-business-justification cr)
         (change-request-implementation-plan cr)
         (change-request-backout-plan cr)
         (change-request-test-plan cr)
         (change-request-communication-plan cr)
         (change-request-external-id cr)
         (change-request-source-system cr)
         (change-request-tags cr)
         (change-request-metadata cr))
        (change-request-id cr))))))

(define (get-change-request id)
  "Retrieve a change request by ID"
  (with-db-connection
   (lambda (conn)
     (let ((results (execute-query conn
                     "SELECT * FROM change_requests WHERE id = ?" id)))
       (if (null? results)
           #f
           (let ((row (car results)))
             (make-change-request
              (list-ref row 0)  ; id
              (list-ref row 1)  ; title
              (list-ref row 2)  ; description
              (list-ref row 3)  ; risk_score
              (list-ref row 4)  ; status
              (list-ref row 5)  ; priority
              (list-ref row 6)  ; category
              (list-ref row 7)  ; impact
              (list-ref row 8)  ; urgency
              (list-ref row 11) ; requestor_id
              (list-ref row 12) ; assignee_id
              (list-ref row 13) ; approver_id
              (list-ref row 14) ; implementer_id
              (list-ref row 9)  ; created_at
              (list-ref row 10) ; updated_at
              (list-ref row 15) ; scheduled_start
              (list-ref row 16) ; scheduled_end
              (list-ref row 17) ; business_justification
              (list-ref row 18) ; implementation_plan
              (list-ref row 19) ; backout_plan
              (list-ref row 20) ; test_plan
              (list-ref row 21) ; communication_plan
              (list-ref row 22) ; external_id
              (list-ref row 23) ; source_system
              (list-ref row 24) ; tags
              (list-ref row 25) ; metadata
              )))))))

(define (update-change-request! id updates)
  "Update a change request with new values"
  (with-db-connection
   (lambda (conn)
     (with-transaction conn
      (lambda (conn)
        (let ((set-clauses '())
              (values '()))

          ;; Build dynamic UPDATE statement
          (for-each
           (lambda (pair)
             (set! set-clauses (cons (format #f "~a = ?" (car pair)) set-clauses))
             (set! values (cons (cdr pair) values)))
           updates)

          (let ((sql (format #f "UPDATE change_requests SET ~a WHERE id = ?"
                             (string-join (reverse set-clauses) ", "))))
            (apply execute-update conn sql (append (reverse values) (list id)))))))))

(define (delete-change-request! id)
  "Delete a change request (soft delete by setting status)"
  (with-db-connection
   (lambda (conn)
     (execute-update conn
      "UPDATE change_requests SET status = 'cancelled' WHERE id = ?" id))))

(define (list-change-requests #:key (status #f) (limit 100) (offset 0))
  "List change requests with optional filtering"
  (with-db-connection
   (lambda (conn)
     (let* ((where-clause (if status
                              "WHERE status = ?"
                              ""))
            (sql (format #f "SELECT * FROM change_requests ~a ORDER BY created_at DESC LIMIT ? OFFSET ?"
                         where-clause))
            (params (if status
                        (list status limit offset)
                        (list limit offset)))
            (results (apply execute-query conn sql params)))

       (map (lambda (row)
              (make-change-request
               (list-ref row 0)  ; id
               (list-ref row 1)  ; title
               (list-ref row 2)  ; description
               (list-ref row 3)  ; risk_score
               (list-ref row 4)  ; status
               (list-ref row 5)  ; priority
               (list-ref row 6)  ; category
               (list-ref row 7)  ; impact
               (list-ref row 8)  ; urgency
               (list-ref row 11) ; requestor_id
               (list-ref row 12) ; assignee_id
               (list-ref row 13) ; approver_id
               (list-ref row 14) ; implementer_id
               (list-ref row 9)  ; created_at
               (list-ref row 10) ; updated_at
               (list-ref row 15) ; scheduled_start
               (list-ref row 16) ; scheduled_end
               (list-ref row 17) ; business_justification
               (list-ref row 18) ; implementation_plan
               (list-ref row 19) ; backout_plan
               (list-ref row 20) ; test_plan
               (list-ref row 21) ; communication_plan
               (list-ref row 22) ; external_id
               (list-ref row 23) ; source_system
               (list-ref row 24) ; tags
               (list-ref row 25) ; metadata
               ))
            results)))))

(define (search-change-requests query)
  "Search change requests by title or description"
  (with-db-connection
   (lambda (conn)
     (let* ((search-pattern (format #f "%~a%" query))
            (results (execute-query conn
                      "SELECT * FROM change_requests
                       WHERE title LIKE ? OR description LIKE ?
                       ORDER BY created_at DESC LIMIT 100"
                      search-pattern search-pattern)))

       (map (lambda (row)
              (make-change-request
               (list-ref row 0)  ; id
               (list-ref row 1)  ; title
               (list-ref row 2)  ; description
               (list-ref row 3)  ; risk_score
               (list-ref row 4)  ; status
               (list-ref row 5)  ; priority
               (list-ref row 6)  ; category
               (list-ref row 7)  ; impact
               (list-ref row 8)  ; urgency
               (list-ref row 11) ; requestor_id
               (list-ref row 12) ; assignee_id
               (list-ref row 13) ; approver_id
               (list-ref row 14) ; implementer_id
               (list-ref row 9)  ; created_at
               (list-ref row 10) ; updated_at
               (list-ref row 15) ; scheduled_start
               (list-ref row 16) ; scheduled_end
               (list-ref row 17) ; business_justification
               (list-ref row 18) ; implementation_plan
               (list-ref row 19) ; backout_plan
               (list-ref row 20) ; test_plan
               (list-ref row 21) ; communication_plan
               (list-ref row 22) ; external_id
               (list-ref row 23) ; source_system
               (list-ref row 24) ; tags
               (list-ref row 25) ; metadata
               ))
            results))))))
