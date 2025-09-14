;;; chaos-orchestrator.scm - Main simulator orchestration
;;; Part of the ITIL Process Simulator - Beautiful Chaos at Scale

(define-module (simulator chaos-orchestrator)
  #:use-module (simulator change-generator)
  #:use-module (simulator failure-injector)
  #:use-module (simulator state-validator)
  #:use-module (simulator metrics)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (run-simulation
            run-chaos-scenario
            simulate-production-day
            generate-simulation-report))

;; Simulation parameters
(define *simulation-config*
  '((changes-per-hour . 10)
    (simulation-hours . 24)
    (chaos-injection-rate . 0.15)
    (validate-compliance . #t)
    (collect-metrics . #t)))

;; Run full simulation
(define* (run-simulation #:key
                         (duration-hours 24)
                         (changes-per-hour 10)
                         (chaos-rate 0.15))
  (format #t "~%🚀 Starting ITIL Chaos Simulation~%")
  (format #t "Duration: ~a hours, Rate: ~a changes/hour, Chaos: ~a%~%"
          duration-hours changes-per-hour (* chaos-rate 100))
  
  ;; Initialize metrics
  (init-metrics)
  
  (let* ((total-changes (* duration-hours changes-per-hour))
         (results '())
         (start-time (current-time)))
    
    ;; Generate and process changes
    (format #t "Generating ~a changes...~%" total-changes)
    (let loop ((hour 0)
               (changes-processed 0))
      (when (< hour duration-hours)
        (format #t "~%Hour ~a/~a:~%" (+ hour 1) duration-hours)
        
        ;; Generate batch of changes for this hour
        (let ((hourly-changes (generate-batch-changes
                                changes-per-hour
                                #:chaos-ratio chaos-rate)))
          
          ;; Process each change
          (for-each
           (lambda (change)
             (let* ((change-id (assoc-ref change 'change-id))
                    (process-start (current-time))
                    
                    ;; Simulate state transitions
                    (states (simulate-state-transitions change))
                    
                    ;; Inject failures based on probability
                    (failure (inject-failure change))
                    
                    ;; Calculate outcome
                    (outcome (if failure 'failed 'success))
                    
                    ;; Generate audit trail
                    (audit-trail (generate-audit-trail change states failure))
                    
                    ;; Validate compliance
                    (compliance (check-compliance change audit-trail))
                    
                    ;; Calculate duration
                    (duration (time-difference (current-time) process-start)))
               
               ;; Record metrics
               (record-change-metric change outcome (time-second duration))
               (when failure
                 (record-failure-metric failure
                                        (assoc-ref failure 'recovery-time)))
               
               ;; Store result
               (set! results
                     (cons `((change . ,change)
                             (outcome . ,outcome)
                             (failure . ,failure)
                             (compliance . ,compliance)
                             (audit-trail . ,audit-trail))
                           results))
               
               ;; Progress indicator
               (when (= (modulo (+ changes-processed 1) 10) 0)
                 (format #t "  [~a/~a] changes processed~%"
                         (+ changes-processed 1) total-changes))))
           hourly-changes)
          
          (loop (+ hour 1)
                (+ changes-processed changes-per-hour)))))
    
    ;; Generate final report
    (let ((end-time (current-time))
          (report (generate-simulation-report results)))
      
      (format #t "~%✅ Simulation Complete!~%")
      (format #t "Total time: ~a seconds~%"
              (time-second (time-difference end-time start-time)))
      (display-report-summary report)
      
      report)))

;; Run specific chaos scenarios
(define (run-chaos-scenario scenario-name)
  (format #t "~%💥 Running Chaos Scenario: ~a~%" scenario-name)
  
  (case scenario-name
    ((friday-afternoon-disaster)
     (simulate-friday-afternoon))
    
    ((cascade-failure)
     (simulate-cascade-failure))
    
    ((peak-load)
     (simulate-peak-load))
    
    ((compliance-nightmare)
     (simulate-compliance-violations))
    
    (else
     (format #t "Unknown scenario: ~a~%" scenario-name))))

;; Simulate a production day
(define (simulate-production-day)
  (format #t "~%📅 Simulating 24-hour production cycle~%")
  
  (let ((hourly-patterns
         '((0 . 2)   ; Midnight - minimal
           (1 . 1)   ; 1 AM
           (2 . 1)   ; 2 AM
           (3 . 1)   ; 3 AM
           (4 . 2)   ; 4 AM
           (5 . 3)   ; 5 AM
           (6 . 5)   ; 6 AM - morning ramp
           (7 . 8)   ; 7 AM
           (8 . 12)  ; 8 AM - morning peak
           (9 . 15)  ; 9 AM - peak
           (10 . 14) ; 10 AM
           (11 . 13) ; 11 AM
           (12 . 8)  ; Noon - lunch dip
           (13 . 7)  ; 1 PM
           (14 . 10) ; 2 PM - afternoon
           (15 . 12) ; 3 PM
           (16 . 11) ; 4 PM
           (17 . 18) ; 5 PM - end of day rush
           (18 . 10) ; 6 PM
           (19 . 6)  ; 7 PM - evening
           (20 . 4)  ; 8 PM
           (21 . 3)  ; 9 PM
           (22 . 2)  ; 10 PM
           (23 . 2)));11 PM
        
        (total-changes 0))
    
    ;; Process each hour with realistic patterns
    (for-each
     (lambda (hour-config)
       (let ((hour (car hour-config))
             (changes (cdr hour-config)))
         (format #t "~%[~2,'0d:00] Processing ~a changes...~%" hour changes)
         
         ;; Generate and process changes for this hour
         (let ((hourly-changes (generate-batch-changes changes)))
           (for-each
            (lambda (change)
              ;; Add time-based context
              (set! change
                    (assoc-set! change 'hour-of-day hour))
              
              ;; Inject more failures during peak hours
              (let ((failure-prob (if (and (>= hour 8) (<= hour 18))
                                       0.20  ; Higher during business hours
                                       0.05)));Lower at night
                
                (when (< (random:uniform) failure-prob)
                  (let ((failure (inject-failure change)))
                    (when failure
                      (format #t "  ⚠️  Failure: ~a - ~a~%"
                              (assoc-ref failure 'failure-type)
                              (assoc-ref failure 'description)))))))
            hourly-changes)
           
           (set! total-changes (+ total-changes changes)))))
     hourly-patterns)
    
    (format #t "~%📊 Day complete: ~a total changes processed~%" total-changes)))

;; Generate comprehensive simulation report
(define (generate-simulation-report results)
  (let* ((total (length results))
         (successful (count (lambda (r) (eq? (assoc-ref r 'outcome) 'success)) results))
         (failed (- total successful))
         (compliant (count (lambda (r)
                             (assoc-ref (assoc-ref r 'compliance) 'compliant))
                           results))
         (violations (fold (lambda (r acc)
                             (append acc
                                     (assoc-ref (assoc-ref r 'compliance) 'violations)))
                           '()
                           results))
         (failures (filter identity
                           (map (lambda (r) (assoc-ref r 'failure)) results))))
    
    `((summary . ((total-changes . ,total)
                  (successful . ,successful)
                  (failed . ,failed)
                  (success-rate . ,(/ successful total))
                  (compliant . ,compliant)
                  (compliance-rate . ,(/ compliant total))))
      
      (failures . ((total . ,(length failures))
                   (by-type . ,(group-failures failures))))
      
      (violations . ((total . ,(length violations))
                     (unique-types . ,(delete-duplicates
                                        (map car violations)))))
      
      (metrics . ,(generate-metrics-report))
      
      (timestamp . ,(current-time)))))

;; Helper functions
(define (simulate-state-transitions change)
  ;; Simulate realistic state transitions
  (let ((states '()))
    (set! states (cons 'draft states))
    (set! states (cons 'submitted states))
    
    ;; Approval based on type
    (when (member (assoc-ref change 'type) '(normal emergency))
      (set! states (cons 'approved states)))
    
    (set! states (cons 'scheduled states))
    (set! states (cons 'in-progress states))
    
    ;; Final state depends on outcome
    (if (assoc-ref change 'has-rollback)
        (set! states (cons 'completed states))
        (when (< (random:uniform) 0.1)
          (set! states (cons 'failed states))))
    
    (set! states (cons 'closed states))
    (reverse states)))

(define (generate-audit-trail change states failure)
  ;; Generate audit entries for each state
  (let ((entries '())
        (current-time (current-time)))
    
    (for-each
     (lambda (state)
       (set! entries
             (cons `((event . ,state)
                     (timestamp . ,current-time)
                     (actor . ,(format #f "system-~a" (random 100)))
                     (change-id . ,(assoc-ref change 'change-id)))
                   entries))
       ;; Advance time
       (set! current-time
             (add-duration current-time
                           (make-time time-duration 0 (* 60 (random 30))))))
     states)
    
    ;; Add failure event if applicable
    (when failure
      (set! entries
            (cons `((event . failure-detected)
                    (timestamp . ,current-time)
                    (details . ,failure))
                  entries)))
    
    (reverse entries)))

(define (group-failures failures)
  ;; Group failures by type
  (let ((types '(network resource dependency configuration human security unknown)))
    (map (lambda (type)
           `(,type . ,(count (lambda (f)
                               (eq? (assoc-ref f 'failure-type) type))
                             failures)))
         types)))

(define (display-report-summary report)
  (let ((summary (assoc-ref report 'summary)))
    (format #t "~%📊 Simulation Summary:~%")
    (format #t "  Total Changes: ~a~%" (assoc-ref summary 'total-changes))
    (format #t "  Success Rate: ~,1f%~%" (* 100 (assoc-ref summary 'success-rate)))
    (format #t "  Compliance Rate: ~,1f%~%" (* 100 (assoc-ref summary 'compliance-rate)))
    (format #t "  Failures: ~a~%" (assoc-ref (assoc-ref report 'failures) 'total))))

;; Specific chaos scenarios
(define (simulate-friday-afternoon)
  (format #t "It's 4:30 PM on Friday...~%")
  (format #t "The CEO wants a 'quick' production update...~%")
  (format #t "What could possibly go wrong?~%~%")
  
  (let ((changes (map (lambda (i)
                        (generate-chaos-change 'friday-afternoon))
                      (iota 5))))
    (for-each
     (lambda (change)
       (format #t "Processing: ~a~%" (assoc-ref change 'title))
       (let ((failure (inject-failure change)))
         (when failure
           (format #t "  💥 FAILED: ~a~%" (assoc-ref failure 'description)))))
     changes)))

(define (simulate-cascade-failure)
  (format #t "One small change, they said...~%")
  (format #t "It'll be fine, they said...~%~%")
  
  (let ((initial-change (generate-chaos-change 'cascade-failure)))
    (format #t "Initial change: ~a~%" (assoc-ref initial-change 'change-id))
    
    ;; Simulate cascading failures
    (let loop ((deps (assoc-ref initial-change 'dependencies))
               (depth 0))
      (when (and (not (null? deps)) (< depth 3))
        (format #t "~%Cascade level ~a:~%" (+ depth 1))
        (for-each
         (lambda (dep)
           (format #t "  Dependency ~a failing...~%" dep)
           (let ((dep-change (generate-realistic-change dep)))
             (inject-failure dep-change)))
         deps)
        (loop (if (< (random:uniform) 0.5)
                  (take deps (max 1 (- (length deps) 1)))
                  '())
              (+ depth 1))))))

(define (simulate-peak-load)
  (format #t "Black Friday deployment window...~%")
  (format #t "100x normal traffic...~%")
  (format #t "No pressure!~%~%")
  
  (let ((changes (generate-batch-changes 50 #:chaos-ratio 0.5)))
    (format #t "Processing ~a simultaneous changes...~%" (length changes))
    
    (let ((failures 0))
      (for-each
       (lambda (change)
         (when (inject-failure change)
           (set! failures (+ failures 1))))
       changes)
      
      (format #t "~%Results: ~a/~a failed (~,1f% failure rate)~%"
              failures (length changes)
              (* 100 (/ failures (length changes)))))))

(define (simulate-compliance-violations)
  (format #t "The auditors are coming...~%")
  (format #t "Quick, make everything compliant!~%~%")
  
  (let ((changes (generate-batch-changes 20)))
    (for-each
     (lambda (change)
       ;; Intentionally create violations
       (set! change (assoc-set! change 'tested-in-staging #f))
       (set! change (assoc-set! change 'has-rollback #f))
       (set! change (assoc-set! change 'approved-by #f))
       
       (let* ((audit-trail (generate-audit-trail change
                                                  '(draft in-progress completed)
                                                  #f))
              (compliance (check-compliance change audit-trail)))
         
         (when (not (assoc-ref compliance 'compliant))
           (format #t "❌ Violation in ~a: ~a~%"
                   (assoc-ref change 'change-id)
                   (assoc-ref compliance 'violations)))))
     changes)))

;; Module initialization
(format #t "~%=== Chaos Orchestrator Module Loaded ===~%")
(format #t "Ready to orchestrate beautiful chaos!~%")
(format #t "Run (run-simulation) to start the chaos~%~%")