#!/usr/bin/env guile
!#

;;; Comprehensive Test Suite for GCF Core Models
;;; Tests all CRUD operations, state transitions, and ITIL compliance

(use-modules (srfi srfi-64)  ; Test framework
             (srfi srfi-1)
             (srfi srfi-19)
             (ice-9 format))

;; Add source directory to load path
(add-to-load-path "../src")

;; Import modules to test
(use-modules (db connection)
             (models change-request)
             (models state-machine))

;; Test configuration
(define *test-db-path* "test-changeflow.db")
(setenv "GCF_DB_PATH" *test-db-path*)

;; Initialize test suite
(test-begin "gcf-core-models-test-suite")

;;; Database Tests
(test-group "database-connection"
  (test-assert "can initialize database"
    (begin
      ;; Clean up any existing test database
      (when (file-exists? *test-db-path*)
        (delete-file *test-db-path*))
      (init-database!)
      (file-exists? *test-db-path*)))

  (test-assert "can get database connection"
    (let ((conn (get-db-connection)))
      (and conn
           (begin
             (close-db-connection conn)
             #t))))

  (test-assert "can execute query"
    (with-db-connection
     (lambda (conn)
       (let ((result (execute-query conn "SELECT 1 as test")))
         (and (not (null? result))
              (= 1 (caar result)))))))

  (test-assert "transaction rollback works"
    (with-db-connection
     (lambda (conn)
       (catch #t
         (lambda ()
           (with-transaction conn
            (lambda (conn)
              (execute-insert conn
               "INSERT INTO users (id, username, email, full_name)
                VALUES ('test-user', 'testuser', 'test@example.com', 'Test User')")
              (error "Forced rollback"))))
         (lambda args #t))
       ;; Check user was not inserted
       (let ((users (execute-query conn
                     "SELECT COUNT(*) FROM users WHERE id = 'test-user'")))
         (zero? (caar users)))))))

;;; Change Request Model Tests
(test-group "change-request-model"
  (test-assert "can generate change ID"
    (let ((id (generate-change-id)))
      (and (string? id)
           (string-prefix? "CHG-" id))))

  (test-assert "can create change request"
    (let* ((id (generate-change-id))
           (cr (make-change-request
                id
                "Test Change"
                "This is a test change request"
                25          ; risk score
                "submitted" ; status
                "medium"    ; priority
                "standard"  ; category
                "low"       ; impact
                "low"       ; urgency
                "user-001"  ; requestor
                #f #f #f    ; assignee, approver, implementer
                (current-date)
                (current-date)
                #f #f       ; scheduled start/end
                "Business justification"
                "Implementation plan"
                "Backout plan"
                "Test plan"
                "Communication plan"
                #f #f #f #f))) ; external id, source, tags, metadata
      (and (change-request? cr)
           (string=? (change-request-id cr) id)
           (= 25 (change-request-risk-score cr)))))

  (test-assert "can validate change request"
    (let ((valid-cr (make-change-request
                      "CHG-001" "Valid" "Description" 50
                      "submitted" "high" "standard" "high" "high"
                      "user-001" #f #f #f
                      (current-date) (current-date)
                      #f #f #f #f #f #f #f #f #f #f #f))
          (invalid-cr (make-change-request
                        "CHG-002" "" "Description" 150  ; Invalid: empty title, risk > 100
                        "invalid-status" "high" "standard" "high" "high"
                        "user-001" #f #f #f
                        (current-date) (current-date)
                        #f #f #f #f #f #f #f #f #f #f #f)))
      (and (validate-change-request valid-cr)
           (not (validate-change-request invalid-cr)))))

  (test-assert "can convert to/from alist"
    (let* ((cr (make-change-request
                "CHG-003" "Test" "Description" 75
                "submitted" "high" "emergency" "critical" "high"
                "user-001" "user-002" #f #f
                (current-date) (current-date)
                #f #f #f #f #f #f #f #f #f #f #f))
           (alist (change-request->alist cr))
           (cr2 (alist->change-request alist)))
      (and (string=? (change-request-id cr) (change-request-id cr2))
           (string=? (change-request-title cr) (change-request-title cr2))
           (= (change-request-risk-score cr) (change-request-risk-score cr2))))))

;;; State Machine Tests
(test-group "state-machine"
  (test-equal "get all states"
    9
    (length (get-all-states)))

  (test-assert "valid transitions"
    (and (valid-transition? 'submitted 'assessing)
         (valid-transition? 'assessing 'approved)
         (valid-transition? 'approved 'implementing)
         (valid-transition? 'implementing 'completed)))

  (test-assert "invalid transitions"
    (and (not (valid-transition? 'submitted 'completed))
         (not (valid-transition? 'completed 'submitted))
         (not (valid-transition? 'rejected 'approved))))

  (test-equal "get next states from submitted"
    '(assessing cancelled)
    (get-next-states 'submitted))

  (test-equal "get next states from completed"
    '()
    (get-next-states 'completed))

  (test-assert "terminal states"
    (and (is-terminal-state? 'completed)
         (is-terminal-state? 'rejected)
         (is-terminal-state? 'cancelled)
         (not (is-terminal-state? 'submitted))
         (not (is-terminal-state? 'implementing))))

  (test-assert "initial state"
    (and (is-initial-state? 'submitted)
         (not (is-initial-state? 'approved))))

  (test-equal "get previous states for approved"
    '(assessing)
    (get-previous-states 'approved))

  (test-assert "state metadata exists"
    (let ((metadata (get-state-metadata 'approved)))
      (and metadata
           (string? (assoc-ref metadata 'description))
           (list? (assoc-ref metadata 'requires))))))

;;; CRUD Operations Tests
(test-group "crud-operations"
  ;; Setup test user
  (with-db-connection
   (lambda (conn)
     (execute-insert conn
      "INSERT OR REPLACE INTO users (id, username, email, full_name, role)
       VALUES ('test-user-crud', 'testuser', 'test@example.com', 'Test User', 'user')")))

  (test-assert "create change request in database"
    (let* ((id (generate-change-id))
           (cr (make-change-request
                id "CRUD Test" "Testing CRUD operations" 30
                "submitted" "low" "standard" "low" "low"
                "test-user-crud" #f #f #f
                (current-date) (current-date)
                #f #f
                "Business case" "Implementation" "Rollback" "Testing" "Comms"
                #f #f #f #f)))
      (create-change-request! cr)
      (let ((retrieved (get-change-request id)))
        (and retrieved
             (string=? (change-request-title retrieved) "CRUD Test")
             (= 30 (change-request-risk-score retrieved))))))

  (test-assert "update change request"
    (let ((id (generate-change-id)))
      ;; Create
      (create-change-request!
       (make-change-request
        id "Original Title" "Original Description" 20
        "submitted" "low" "standard" "low" "low"
        "test-user-crud" #f #f #f
        (current-date) (current-date)
        #f #f #f #f #f #f #f #f #f #f #f))
      ;; Update
      (update-change-request! id
       '((title . "Updated Title")
         (risk_score . 50)
         (priority . "high")))
      ;; Verify
      (let ((updated (get-change-request id)))
        (and (string=? (change-request-title updated) "Updated Title")
             (= 50 (change-request-risk-score updated))
             (string=? (change-request-priority updated) "high")))))

  (test-assert "list change requests"
    (let ((all-changes (list-change-requests))
          (submitted (list-change-requests #:status "submitted")))
      (and (list? all-changes)
           (list? submitted)
           (>= (length all-changes) (length submitted)))))

  (test-assert "search change requests"
    (let ((id (generate-change-id)))
      (create-change-request!
       (make-change-request
        id "Searchable Change" "Contains unique keyword XYZABC123" 40
        "submitted" "medium" "standard" "medium" "medium"
        "test-user-crud" #f #f #f
        (current-date) (current-date)
        #f #f #f #f #f #f #f #f #f #f #f))
      (let ((results (search-change-requests "XYZABC123")))
        (and (not (null? results))
             (string=? (change-request-id (car results)) id)))))

  (test-assert "soft delete change request"
    (let ((id (generate-change-id)))
      (create-change-request!
       (make-change-request
        id "To Delete" "Will be cancelled" 10
        "submitted" "low" "standard" "low" "low"
        "test-user-crud" #f #f #f
        (current-date) (current-date)
        #f #f #f #f #f #f #f #f #f #f #f))
      (delete-change-request! id)
      (let ((deleted (get-change-request id)))
        (string=? (change-request-status deleted) "cancelled")))))

;;; State Transition Tests
(test-group "state-transitions"
  ;; Setup test users
  (with-db-connection
   (lambda (conn)
     (execute-insert conn
      "INSERT OR REPLACE INTO users (id, username, email, full_name, role)
       VALUES ('approver-001', 'approver', 'approver@example.com', 'Approver User', 'approver')")))

  (test-assert "record state transition"
    (let ((id (generate-change-id)))
      (create-change-request!
       (make-change-request
        id "Transition Test" "Testing transitions" 35
        "submitted" "medium" "standard" "medium" "medium"
        "test-user-crud" #f #f #f
        (current-date) (current-date)
        #f #f #f #f #f #f #f #f #f #f #f))

      (record-transition! id #f "submitted" "test-user-crud" "Initial creation")

      (let ((history (get-transition-history id)))
        (and (not (null? history))
             (string=? (assoc-ref (car history) 'to_state) "submitted")))))

  (test-assert "transition with validation"
    (let ((id (generate-change-id)))
      (create-change-request!
       (make-change-request
        id "Validation Test" "Testing transition validation" 45
        "submitted" "high" "standard" "high" "high"
        "test-user-crud" #f #f #f
        (current-date) (current-date)
        #f #f #f #f #f #f #f #f #f #f #f))

      ;; Should fail - no risk assessment
      (catch #t
        (lambda ()
          (transition-state! id "submitted" "assessing" "test-user-crud" "Moving to assessment")
          #f)
        (lambda args #t)))))

;;; Audit Trail Tests
(test-group "audit-trail"
  (test-assert "audit log captures inserts"
    (let ((id (generate-change-id)))
      (create-change-request!
       (make-change-request
        id "Audit Test" "Testing audit trail" 60
        "submitted" "high" "standard" "high" "high"
        "test-user-crud" #f #f #f
        (current-date) (current-date)
        #f #f #f #f #f #f #f #f #f #f #f))

      (with-db-connection
       (lambda (conn)
         (let ((audit-entries (execute-query conn
                               "SELECT * FROM audit_log
                                WHERE table_name = 'change_requests'
                                AND record_id = ?
                                AND action = 'INSERT'"
                               id)))
           (not (null? audit-entries)))))))

  (test-assert "audit log captures updates"
    (let ((id (generate-change-id)))
      (create-change-request!
       (make-change-request
        id "Update Audit Test" "Testing update audit" 70
        "submitted" "medium" "standard" "medium" "medium"
        "test-user-crud" #f #f #f
        (current-date) (current-date)
        #f #f #f #f #f #f #f #f #f #f #f))

      (update-change-request! id '((title . "Updated for Audit")))

      (with-db-connection
       (lambda (conn)
         (let ((audit-entries (execute-query conn
                               "SELECT * FROM audit_log
                                WHERE table_name = 'change_requests'
                                AND record_id = ?
                                AND action = 'UPDATE'"
                               id)))
           (not (null? audit-entries))))))))

;;; Performance Tests
(test-group "performance"
  (test-assert "bulk insert performance"
    (let ((start-time (current-time)))
      ;; Insert 100 change requests
      (do ((i 0 (+ i 1)))
          ((= i 100))
        (create-change-request!
         (make-change-request
          (format #f "PERF-~4,'0d" i)
          (format #f "Performance Test ~a" i)
          "Bulk insert test"
          (random 100)
          "submitted" "low" "standard" "low" "low"
          "test-user-crud" #f #f #f
          (current-date) (current-date)
          #f #f #f #f #f #f #f #f #f #f #f)))

      (let* ((end-time (current-time))
             (duration (time-difference end-time start-time))
             (seconds (time-second duration)))
        ;; Should complete in under 10 seconds
        (< seconds 10))))

  (test-assert "query performance"
    (let ((start-time (current-time)))
      ;; Run 100 queries
      (do ((i 0 (+ i 1)))
          ((= i 100))
        (list-change-requests #:limit 10))

      (let* ((end-time (current-time))
             (duration (time-difference end-time start-time))
             (seconds (time-second duration)))
        ;; Should complete in under 5 seconds
        (< seconds 5)))))

;;; ITIL Compliance Tests
(test-group "itil-compliance"
  (test-assert "change categories are ITIL compliant"
    (let ((categories '("standard" "normal" "emergency" "pre-approved")))
      (every (lambda (cat)
               (let* ((id (generate-change-id))
                      (cr (make-change-request
                           id "ITIL Test" "Testing ITIL compliance" 50
                           "submitted" "medium" cat "medium" "medium"
                           "test-user-crud" #f #f #f
                           (current-date) (current-date)
                           #f #f #f #f #f #f #f #f #f #f #f)))
                 (validate-change-request cr)))
             categories)))

  (test-assert "priority levels are ITIL compliant"
    (let ((priorities '("low" "medium" "high" "critical")))
      (every (lambda (priority)
               (let* ((id (generate-change-id))
                      (cr (make-change-request
                           id "Priority Test" "Testing priority" 50
                           "submitted" priority "standard" "medium" "medium"
                           "test-user-crud" #f #f #f
                           (current-date) (current-date)
                           #f #f #f #f #f #f #f #f #f #f #f)))
                 (validate-change-request cr)))
             priorities)))

  (test-assert "impact and urgency matrix"
    (let ((levels '("low" "medium" "high" "critical")))
      (every (lambda (impact)
               (every (lambda (urgency)
                        (let* ((id (generate-change-id))
                               (cr (make-change-request
                                    id "Matrix Test" "Testing impact/urgency" 50
                                    "submitted" "medium" "standard" impact urgency
                                    "test-user-crud" #f #f #f
                                    (current-date) (current-date)
                                    #f #f #f #f #f #f #f #f #f #f #f)))
                          (validate-change-request cr)))
                      levels))
             levels))))

;; End test suite
(test-end "gcf-core-models-test-suite")

;; Generate test report
(let ((runner (test-runner-current)))
  (format #t "~%Test Summary:~%")
  (format #t "  Total tests: ~a~%" (test-runner-test-count runner))
  (format #t "  Passed: ~a~%" (test-runner-pass-count runner))
  (format #t "  Failed: ~a~%" (test-runner-fail-count runner))
  (format #t "  Skipped: ~a~%" (test-runner-skip-count runner))

  (when (> (test-runner-fail-count runner) 0)
    (format #t "~%TESTS FAILED!~%")
    (exit 1))

  (format #t "~%ALL TESTS PASSED!~%")
  (exit 0))