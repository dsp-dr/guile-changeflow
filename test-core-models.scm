#!/usr/bin/env guile
!#

(add-to-load-path "src")

(use-modules (models change-request)
             (models state-machine)
             (models approval)
             (models audit)
             (storage memory))

(define (test-basic-crud)
  (display "Testing basic CRUD operations...\n")

  ;; Initialize storage
  (init-storage!)

  ;; Test creating a change request
  (define test-change
    (create-and-store-change! "Test Change Request"
                              "This is a test change for the system"
                              #:risk-score 25
                              #:submitter "john.doe"
                              #:impact-level 'high
                              #:urgency 'normal
                              #:category 'standard))

  (display "✓ Created change request: ")
  (display (change-request-id test-change))
  (newline)

  ;; Test retrieving the change
  (define retrieved-change (get-change (change-request-id test-change)))
  (if (and retrieved-change
           (string=? (change-request-title retrieved-change) "Test Change Request"))
      (display "✓ Successfully retrieved change request\n")
      (display "✗ Failed to retrieve change request\n"))

  ;; Test updating the change
  (update-change! (change-request-id test-change)
                  (lambda (change)
                    (set-change-request-risk-score! change 50)))

  (define updated-change (get-change (change-request-id test-change)))
  (if (= (change-request-risk-score updated-change) 50)
      (display "✓ Successfully updated change request\n")
      (display "✗ Failed to update change request\n"))

  ;; Test listing all changes
  (define all-changes (get-all-changes))
  (if (= (length all-changes) 1)
      (display "✓ Retrieved all changes correctly\n")
      (display "✗ Failed to retrieve all changes\n"))

  test-change)

(define (test-state-transitions change)
  (display "\nTesting state transitions...\n")

  ;; Test valid transition: submitted -> assessing
  (if (valid-transition? 'submitted 'assessing)
      (display "✓ Valid transition check passed\n")
      (display "✗ Valid transition check failed\n"))

  ;; Test invalid transition
  (if (not (valid-transition? 'submitted 'completed))
      (display "✓ Invalid transition check passed\n")
      (display "✗ Invalid transition check failed\n"))

  ;; Test actual state transition
  (transition-change! (change-request-id change) 'assessing "system")
  (define transitioned-change (get-change (change-request-id change)))
  (if (eq? (change-request-status transitioned-change) 'assessing)
      (display "✓ State transition successful\n")
      (display "✗ State transition failed\n"))

  transitioned-change)

(define (test-approval-workflow change)
  (display "\nTesting approval workflow...\n")

  ;; Test approval
  (define approval-result
    (approve-change! change "jane.smith" "This change looks good to me"))

  (if (approval? approval-result)
      (display "✓ Change approval successful\n")
      (display "✗ Change approval failed\n"))

  ;; Check if change status changed to approved
  (define approved-change (get-change (change-request-id change)))
  (if (eq? (change-request-status approved-change) 'approved)
      (display "✓ Change status updated to approved\n")
      (display "✗ Change status not updated correctly\n"))

  approved-change)

(define (test-audit-trail change)
  (display "\nTesting audit trail...\n")

  ;; Get audit trail for the change
  (define audit-entries (get-audit-trail (change-request-id change)))

  (if (> (length audit-entries) 0)
      (begin
        (display "✓ Audit trail created: ")
        (display (length audit-entries))
        (display " entries\n")

        ;; Display audit entries
        (for-each (lambda (entry)
                    (display "  - ")
                    (display (audit-entry-event-type entry))
                    (display ": ")
                    (display (audit-entry-details entry))
                    (newline))
                  audit-entries))
      (display "✗ No audit trail found\n")))

(define (test-storage-stats)
  (display "\nStorage statistics:\n")
  (define stats (storage-stats))
  (for-each (lambda (stat)
              (display "  ")
              (display (car stat))
              (display ": ")
              (display (cdr stat))
              (newline))
            stats))

(define (run-all-tests)
  (display "=== Core Models Test Suite ===\n")

  (let* ((change (test-basic-crud))
         (transitioned-change (test-state-transitions change))
         (approved-change (test-approval-workflow transitioned-change)))

    (test-audit-trail approved-change)
    (test-storage-stats)

    (display "\n=== Test Suite Complete ===\n")))

;; Run the tests
(run-all-tests)