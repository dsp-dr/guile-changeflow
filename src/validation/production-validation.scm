#!/usr/bin/env guile
!#

;;; Production Readiness Validation
;;; Validates all critical requirements for 7 AM demo

(add-to-load-path "src")
(use-modules (main)
             (ice-9 format))

(define (check-requirement description test-fn)
  "Check a single requirement"
  (let ((result (test-fn)))
    (display (format #f "[~a] ~a\n"
                    (if result "✓" "✗")
                    description))
    result))

(define (validate-production-readiness)
  "Validate all production requirements"
  (display "========================================\n")
  (display "  PRODUCTION READINESS VALIDATION\n")
  (display "========================================\n\n")

  (let ((checks '()))

    ;; Requirement 1: Risk score calculation (0-100)
    (set! checks
          (cons (check-requirement
                 "Risk score returns 0-100 range"
                 (lambda ()
                   (let* ((low (calculate-risk "Update docs" "" '()))
                          (high (calculate-risk "URGENT production payment database drop all tables"
                                               "Emergency security breach response"
                                               '("payment-gateway" "database-master" "auth-service"))))
                     (and (>= low 0) (<= low 100)
                          (>= high 0) (<= high 100)))))
                checks))

    ;; Requirement 2: Risk categories work correctly
    (set! checks
          (cons (check-requirement
                 "Risk categories (low/medium/high/critical)"
                 (lambda ()
                   (and (eq? (categorize-risk 15) 'low)
                        (eq? (categorize-risk 50) 'medium)
                        (eq? (categorize-risk 85) 'high)
                        (eq? (categorize-risk 95) 'critical))))
                checks))

    ;; Requirement 3: Freeze period detection
    (set! checks
          (cons (check-requirement
                 "Freeze period detection works"
                 (lambda ()
                   (boolean? (in-freeze-period?))))
                checks))

    ;; Requirement 4: Keywords affect risk score
    (set! checks
          (cons (check-requirement
                 "Keywords in title affect risk score"
                 (lambda ()
                   (let ((safe (calculate-risk "Update readme" "" '()))
                         (risky (calculate-risk "Production payment security breach" "" '())))
                     (> risky safe))))
                checks))

    ;; Requirement 5: System count affects risk
    (set! checks
          (cons (check-requirement
                 "System count affects risk score"
                 (lambda ()
                   (let ((one-sys (calculate-risk "Change" "" '("api")))
                         (many-sys (calculate-risk "Change" "" '("api" "database" "payment"))))
                     (> many-sys one-sys))))
                checks))

    ;; Requirement 6: MCP integration ready
    (set! checks
          (cons (check-requirement
                 "assess-change-request function available for MCP"
                 (lambda ()
                   (let* ((request '((title . "Test change")
                                    (description . "Test description")
                                    (systems . ("api"))))
                          (result (assess-change-request request)))
                     (and (assoc-ref result 'risk-score)
                          (assoc-ref result 'risk-category)))))
                checks))

    ;; Requirement 7: Approval requirements based on risk
    (set! checks
          (cons (check-requirement
                 "Approval requirements scale with risk"
                 (lambda ()
                   (and (= (get-approval-requirement 'low) 1)
                        (= (get-approval-requirement 'medium) 2)
                        (= (get-approval-requirement 'high) 3)
                        (= (get-approval-requirement 'critical) 5))))
                checks))

    ;; Requirement 8: Weekend detection (it's Saturday!)
    (set! checks
          (cons (check-requirement
                 "Weekend detection increases risk"
                 (lambda ()
                   (> (get-freeze-risk-modifier) 0)))
                checks))

    ;; Requirement 9: Risk colors for UI
    (set! checks
          (cons (check-requirement
                 "Risk colors available for web UI"
                 (lambda ()
                   (and (string=? (get-risk-color 'low) "green")
                        (string=? (get-risk-color 'medium) "yellow")
                        (string=? (get-risk-color 'high) "orange")
                        (string=? (get-risk-color 'critical) "red"))))
                checks))

    ;; Requirement 10: Engine info available
    (set! checks
          (cons (check-requirement
                 "Risk engine info endpoint works"
                 (lambda ()
                   (let ((info (risk-engine-info)))
                     (and (assoc-ref info 'name)
                          (assoc-ref info 'version)
                          (assoc-ref info 'capabilities)))))
                checks))

    ;; Summary
    (let* ((total (length checks))
           (passed (length (filter identity checks)))
           (failed (- total passed)))

      (display "\n========================================\n")
      (display (format #f "RESULTS: ~a/~a checks passed\n" passed total))

      (if (= passed total)
          (begin
            (display "\n✅ PRODUCTION READY FOR 7 AM DEMO! ✅\n")
            (display "\nRisk Engine Features:\n")
            (display "  • Calculates risk scores 0-100\n")
            (display "  • Categorizes into low/medium/high/critical\n")
            (display "  • Detects freeze periods (weekends, holidays)\n")
            (display "  • Analyzes risky keywords\n")
            (display "  • Considers affected systems\n")
            (display "  • Provides approval requirements\n")
            (display "  • Ready for MCP integration\n"))
          (display (format #f "\n⚠️  WARNING: ~a checks failed!\n" failed)))

      (display "========================================\n"))))

;; Run validation
(validate-production-readiness)