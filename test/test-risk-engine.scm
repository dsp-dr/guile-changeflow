#!/usr/bin/env guile
!#

;;; Test the Risk Engine
;;; Run with: guile test-risk-engine.scm

(add-to-load-path "src")
(use-modules (ice-9 format))

;; Load the main module after adding to load path
(use-modules (main))

(define (display-test title result)
  "Display test result nicely"
  (display "=====================================\n")
  (display (format #f "Test: ~a\n" title))
  (display "-------------------------------------\n")
  (for-each (lambda (pair)
              (display (format #f "  ~a: ~a\n" (car pair) (cdr pair))))
            result)
  (newline))

(define (run-tests)
  "Run various risk assessment tests"
  (display "GCF Risk Engine Test Suite\n")
  (display "=====================================\n\n")

  ;; Test 1: Low risk scenario
  (display-test "Low Risk - Documentation Update"
                (assess-risk "Update README documentation"
                            "Fix typos in the installation guide"
                            '()))

  ;; Test 2: Medium risk scenario
  (display-test "Medium Risk - API Update"
                (assess-risk "Update API endpoints"
                            "Add new REST endpoints for user management"
                            '("api-gateway")))

  ;; Test 3: High risk scenario
  (display-test "High Risk - Database Migration"
                (assess-risk "Production database migration"
                            "Migrate user table schema"
                            '("database-master" "api-gateway")))

  ;; Test 4: Critical risk scenario
  (display-test "Critical Risk - Emergency Security Fix"
                (assess-risk "URGENT: Security patch for payment system"
                            "Critical vulnerability in payment processing"
                            '("payment-gateway" "auth-service" "database-master")))

  ;; Test 5: Freeze period check
  (display "Freeze Period Status:\n")
  (display (format #f "  Currently in freeze: ~a\n" (in-freeze-period?)))
  (display (format #f "  Next window: ~a\n" (get-next-window)))
  (display (format #f "  Blackout dates: ~a\n" (check-blackout-dates)))
  (newline)

  ;; Test 6: Risk factors
  (display "Risk Factor Weights:\n")
  (display (format #f "  production: ~a\n" (get-factor-weight 'production)))
  (display (format #f "  security: ~a\n" (get-factor-weight 'security)))
  (display (format #f "  payment: ~a\n" (get-factor-weight 'payment)))
  (display (format #f "  database: ~a\n" (get-factor-weight 'database)))
  (newline)

  ;; Test 7: Engine info
  (display "Risk Engine Info:\n")
  (for-each (lambda (pair)
              (display (format #f "  ~a: ~a\n" (car pair) (cdr pair))))
            (risk-engine-info))
  (newline)

  (display "=====================================\n")
  (display "All tests completed!\n"))

;; Run the tests
(run-tests)