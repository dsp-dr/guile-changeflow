#!/usr/bin/env guile
!#

;; Comprehensive Guile Module Test Suite for ChangeFlow
;; Tests all modules for 7 AM demo readiness

(define-module (test guile-module-test)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1))

;; Test result tracking
(define test-results '())
(define (add-test-result module-name success? error-msg)
  (set! test-results
    (cons (list module-name success? error-msg) test-results)))

;; Test a single module
(define (test-module module-name)
  (display (string-append "Testing " (symbol->string module-name) "... "))
  (catch #t
    (lambda ()
      (eval `(use-modules (,module-name)) (interaction-environment))
      (display "✓ SUCCESS\n")
      (add-test-result module-name #t ""))
    (lambda (key . args)
      (display "❌ FAILED\n")
      (add-test-result module-name #f (format #f "~a: ~a" key args)))))

;; Test all modules
(define (run-all-tests)
  (display "🧪 GUILE CHANGEFLOW MODULE TEST SUITE\n")
  (display "=====================================\n\n")

  ;; Models
  (display "📊 Testing Models:\n")
  (test-module '(models change-request))
  (test-module '(models audit))
  (test-module '(models approval))
  (test-module '(models state-machine))

  ;; MCP
  (display "\n🔗 Testing MCP:\n")
  (test-module '(mcp server))
  (test-module '(mcp tools))
  (test-module '(mcp handlers))
  (test-module '(mcp discovery))

  ;; Risk Engine
  (display "\n⚠️ Testing Risk Engine:\n")
  (test-module '(risk calculator))
  (test-module '(risk factors))
  (test-module '(risk freeze))
  (test-module '(risk categories))

  ;; Integrations
  (display "\n🔌 Testing Integrations:\n")
  (test-module '(integrations router))
  (test-module '(integrations notifications))
  (test-module '(integrations github))
  (test-module '(integrations mock))

  ;; Web
  (display "\n🌐 Testing Web:\n")
  (test-module '(web server))
  (test-module '(web api))
  (test-module '(web dashboard))

  ;; Storage
  (display "\n💾 Testing Storage:\n")
  (test-module '(storage memory))

  ;; Webhooks
  (display "\n🪝 Testing Webhooks:\n")
  (test-module '(webhooks server))

  ;; Summary
  (display "\n📈 TEST SUMMARY:\n")
  (display "================\n")
  (let* ((total (length test-results))
         (successes (length (filter (lambda (r) (cadr r)) test-results)))
         (failures (- total successes)))
    (display (format #f "Total modules tested: ~a\n" total))
    (display (format #f "✓ Successful: ~a\n" successes))
    (display (format #f "❌ Failed: ~a\n" failures))
    (display (format #f "Success rate: ~a%\n"
                     (round (* 100 (/ successes total)))))

    (when (> failures 0)
      (display "\n🐛 FAILED MODULES:\n")
      (for-each (lambda (result)
                  (when (not (cadr result))
                    (display (format #f "- ~a: ~a\n"
                                     (car result) (caddr result)))))
                (reverse test-results)))))

;; Run the tests
(run-all-tests)