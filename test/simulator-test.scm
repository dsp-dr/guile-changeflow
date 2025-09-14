#!/usr/bin/env guile
!#

;;; simulator-test.scm - Test harness for ITIL Process Simulator
;;; Run with: guile -L src test/simulator-test.scm

(use-modules (simulator chaos-orchestrator)
             (simulator change-generator)
             (simulator failure-injector)
             (simulator state-validator)
             (simulator metrics)
             (ice-9 format))

(define (run-test name test-fn)
  (format #t "~%🧪 Testing: ~a~%" name)
  (format #t "==================================~%")
  (catch #t
    (lambda ()
      (test-fn)
      (format #t "✅ ~a: PASSED~%" name))
    (lambda (key . args)
      (format #t "❌ ~a: FAILED - ~a~%" name args))))

;; Test change generation
(define (test-change-generation)
  (format #t "Generating realistic change...~%")
  (let ((change (generate-realistic-change)))
    (assert (assoc-ref change 'change-id))
    (assert (assoc-ref change 'type))
    (assert (assoc-ref change 'risk-level))
    (format #t "  Generated: ~a~%" (assoc-ref change 'change-id)))
  
  (format #t "Generating chaos change...~%")
  (let ((chaos (generate-chaos-change 'friday-afternoon)))
    (assert (eq? (assoc-ref chaos 'type) 'emergency))
    (assert (eq? (assoc-ref chaos 'risk-level) 'critical))
    (format #t "  Chaos type: ~a~%" (assoc-ref chaos 'chaos-type)))
  
  (format #t "Generating batch...~%")
  (let ((batch (generate-batch-changes 10 #:chaos-ratio 0.2)))
    (assert (= (length batch) 10))
    (format #t "  Batch size: ~a~%" (length batch))))

;; Test failure injection
(define (test-failure-injection)
  (format #t "Testing failure probability calculation...~%")
  (let* ((high-risk-change '((risk-level . critical)
                             (environment . production)
                             (tested-in-staging . #f)
                             (has-rollback . #f)
                             (time-pressure . 1.5)))
         (low-risk-change '((risk-level . low)
                            (environment . dev)
                            (tested-in-staging . #t)
                            (has-rollback . #t)
                            (time-pressure . 1.0)))
         (high-prob (calculate-failure-probability high-risk-change))
         (low-prob (calculate-failure-probability low-risk-change)))
    
    (assert (> high-prob low-prob))
    (format #t "  High risk probability: ~,2f~%" high-prob)
    (format #t "  Low risk probability: ~,2f~%" low-prob))
  
  (format #t "Simulating network failure...~%")
  (let ((failure (simulate-network-issues '((change-id . "TEST-001")
                                             (components-affected . 5)
                                             (has-rollback . #t)))))
    (assert (eq? (assoc-ref failure 'failure-type) 'network))
    (assert (assoc-ref failure 'recovery-time))
    (format #t "  Recovery time: ~a seconds~%" (assoc-ref failure 'recovery-time))))

;; Test state validation
(define (test-state-validation)
  (format #t "Testing valid state transitions...~%")
  (let ((valid (validate-state-transition 'draft 'submitted 'normal))
        (invalid (validate-state-transition 'draft 'completed 'normal)))
    
    (assert (assoc-ref valid 'valid))
    (assert (not (assoc-ref invalid 'valid)))
    (format #t "  Valid transition: draft -> submitted~%")
    (format #t "  Invalid transition: draft -> completed~%"))
  
  (format #t "Testing audit trail validation...~%")
  (let* ((good-trail '(((event . created) (timestamp . 1) (actor . "user1"))
                       ((event . submitted) (timestamp . 2) (actor . "user1"))
                       ((event . approved) (timestamp . 3) (actor . "cab"))
                       ((event . completed) (timestamp . 4) (actor . "system"))))
         (bad-trail '(((event . created) (timestamp . 1))
                      ((event . completed) (timestamp . 2) (actor . "user1"))))
         (good-result (validate-audit-trail good-trail '()))
         (bad-result (validate-audit-trail bad-trail '())))
    
    (assert (not (assoc-ref good-result 'complete)))  ; Missing required events
    (assert (not (assoc-ref bad-result 'has-actors)))  ; Missing actor
    (format #t "  Audit validation working~%")))

;; Test metrics collection
(define (test-metrics)
  (format #t "Initializing metrics...~%")
  (init-metrics)
  
  (format #t "Recording test metrics...~%")
  (record-change-metric '((change-id . "TEST-001")
                          (type . normal)
                          (environment . production)
                          (risk-level . medium)
                          (components-affected . 5))
                        'success
                        300)
  
  (record-change-metric '((change-id . "TEST-002")
                          (type . emergency)
                          (environment . production)
                          (risk-level . critical)
                          (components-affected . 10))
                        'failed
                        600)
  
  (record-failure-metric '((failure-type . network)
                           (change-id . "TEST-002")
                           (severity . critical)
                           (timestamp . 1234567890)
                           (impact . "Service down"))
                         1800)
  
  (format #t "Calculating metrics...~%")
  (let ((success-rate (calculate-success-rate))
        (mttr (calculate-mttr)))
    
    (assert (= success-rate 0.5))  ; 1 success, 1 failure
    (assert (= mttr 1800))  ; One failure with 1800s recovery
    (format #t "  Success rate: ~,1f%~%" (* 100 success-rate))
    (format #t "  MTTR: ~a seconds~%" mttr))
  
  (format #t "Generating report...~%")
  (let ((report (generate-metrics-report)))
    (assert (assoc-ref report 'summary))
    (assert (assoc-ref report 'by-type))
    (format #t "  Report generated with ~a sections~%" (length report))))

;; Test mini simulation
(define (test-mini-simulation)
  (format #t "Running mini simulation (5 changes)...~%")
  (let ((report (run-simulation #:duration-hours 1
                                #:changes-per-hour 5
                                #:chaos-rate 0.2)))
    
    (assert (assoc-ref report 'summary))
    (let ((summary (assoc-ref report 'summary)))
      (format #t "  Total changes: ~a~%" (assoc-ref summary 'total-changes))
      (format #t "  Success rate: ~,1f%~%" (* 100 (assoc-ref summary 'success-rate)))
      (format #t "  Compliance rate: ~,1f%~%" (* 100 (assoc-ref summary 'compliance-rate))))))

;; Test chaos scenarios
(define (test-chaos-scenarios)
  (format #t "Testing Friday afternoon scenario...~%")
  (run-chaos-scenario 'friday-afternoon-disaster)
  
  (format #t "~%Testing cascade failure scenario...~%")
  (run-chaos-scenario 'cascade-failure)
  
  (format #t "~%Testing peak load scenario...~%")
  (run-chaos-scenario 'peak-load))

;; Helper assertion
(define (assert condition)
  (if (not condition)
      (error "Assertion failed!")))

;; Main test runner
(define (main)
  (format #t "~%🚀 ITIL Process Simulator Test Suite~%")
  (format #t "========================================~%")
  
  (run-test "Change Generation" test-change-generation)
  (run-test "Failure Injection" test-failure-injection)
  (run-test "State Validation" test-state-validation)
  (run-test "Metrics Collection" test-metrics)
  (run-test "Mini Simulation" test-mini-simulation)
  (run-test "Chaos Scenarios" test-chaos-scenarios)
  
  (format #t "~%~%🎆 All tests completed!~%")
  (format #t "The simulator is ready to create beautiful chaos!~%~%"))

;; Run tests
(main)