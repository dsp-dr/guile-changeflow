#!/usr/bin/env guile
!#

;; Simple module test for demo readiness
(use-modules (ice-9 format))

(define (test-module-load module-name)
  (catch #t
    (lambda ()
      (eval `(use-modules ,module-name) (interaction-environment))
      (format #t "✓ ~a loaded successfully\n" module-name)
      #t)
    (lambda (key . args)
      (format #t "❌ ~a failed: ~a\n" module-name (car args))
      #f)))

(format #t "🧪 GUILE CHANGEFLOW DEMO READINESS TEST\n")
(format #t "======================================\n\n")

(define total-tests 0)
(define passed-tests 0)

(define (run-test name module)
  (set! total-tests (+ total-tests 1))
  (format #t "Testing ~a... " name)
  (when (test-module-load module)
    (set! passed-tests (+ passed-tests 1))))

;; Core Models
(format #t "📊 MODELS:\n")
(run-test "Change Requests" '(models change-request))
(run-test "Audit System" '(models audit))
(run-test "Approvals" '(models approval))
(run-test "State Machine" '(models state-machine))

;; MCP Protocol
(format #t "\n🔗 MCP PROTOCOL:\n")
(run-test "MCP Server" '(mcp server))
(run-test "MCP Tools" '(mcp tools))
(run-test "MCP Handlers" '(mcp handlers))
(run-test "MCP Discovery" '(mcp discovery))

;; Risk Engine
(format #t "\n⚠️ RISK ENGINE:\n")
(run-test "Risk Calculator" '(risk calculator))
(run-test "Risk Factors" '(risk factors))
(run-test "Freeze Periods" '(risk freeze))
(run-test "Risk Categories" '(risk categories))

;; Summary
(format #t "\n📈 SUMMARY:\n")
(format #t "===========\n")
(format #t "Total modules: ~a\n" total-tests)
(format #t "✓ Passed: ~a\n" passed-tests)
(format #t "❌ Failed: ~a\n" (- total-tests passed-tests))
(format #t "Success rate: ~a%\n"
        (if (> total-tests 0)
            (round (* 100 (/ passed-tests total-tests)))
            0))

(if (= passed-tests total-tests)
    (format #t "\n🚀 ALL SYSTEMS GREEN FOR 7 AM DEMO!\n")
    (format #t "\n⚠️ Issues found - check failed modules\n"))