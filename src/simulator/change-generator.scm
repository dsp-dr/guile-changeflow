;;; change-generator.scm - Generate realistic change patterns for simulation
;;; Part of the ITIL Process Simulator - Beautiful Chaos at Scale

(define-module (simulator change-generator)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)   ; Time operations
  #:use-module (srfi srfi-27)   ; Random numbers
  #:export (generate-realistic-change
            generate-chaos-change
            generate-edge-case
            generate-batch-changes
            init-random-seed))

;; Initialize random seed
(define (init-random-seed)
  (random-state-from-platform))

;; Current random state
(define *random-state* (init-random-seed))

;; Change type distributions (realistic)
(define change-type-weights
  '((standard . 0.70)    ; 70% standard changes
    (normal . 0.25)      ; 25% normal changes
    (emergency . 0.05))) ; 5% emergency changes

;; Risk level distributions
(define risk-level-weights
  '((low . 0.60)
    (medium . 0.30)
    (high . 0.08)
    (critical . 0.02)))

;; Environment distributions
(define environment-weights
  '((dev . 0.40)
    (test . 0.30)
    (staging . 0.20)
    (production . 0.10)))

;; Time-based patterns (hour of day affects change patterns)
(define (get-time-based-multiplier hour)
  (cond
    ((< hour 6) 0.1)        ; Night - minimal changes
    ((< hour 9) 0.8)        ; Early morning - ramping up
    ((< hour 12) 1.2)       ; Morning - peak activity
    ((< hour 14) 0.6)       ; Lunch - reduced activity
    ((< hour 17) 1.0)       ; Afternoon - normal activity
    ((< hour 18) 1.5)       ; End of day - rush to deploy
    ((< hour 20) 0.7)       ; Evening - winding down
    (else 0.3)))            ; Late night - emergency only

;; Select weighted random item
(define (weighted-random items)
  (let* ((total (apply + (map cdr items)))
         (r (random:uniform *random-state*))
         (target (* r total)))
    (let loop ((items items)
               (sum 0))
      (if (null? items)
          'standard  ; Default fallback
          (let ((new-sum (+ sum (cdar items))))
            (if (< target new-sum)
                (caar items)
                (loop (cdr items) new-sum)))))))

;; Generate realistic change request
(define* (generate-realistic-change #:optional (change-id #f))
  (let* ((id (or change-id (format #f "CHG-SIM-~a" (random 100000 *random-state*))))
         (type (weighted-random change-type-weights))
         (risk-level (weighted-random risk-level-weights))
         (environment (weighted-random environment-weights))
         (current-hour (date-hour (current-date)))
         (time-multiplier (get-time-based-multiplier current-hour)))

    `((change-id . ,id)
      (type . ,type)
      (title . ,(format #f "~a deployment - ~a"
                        (case type
                          ((standard) "Routine configuration update")
                          ((normal) "Service enhancement")
                          ((emergency) "CRITICAL: Security patch"))
                        id))
      (description . ,(format #f "Simulated ~a change for ~a environment"
                              type environment))
      (risk-level . ,risk-level)
      (environment . ,environment)
      (components-affected . ,(+ 1 (random 10 *random-state*)))
      (has-rollback . ,(> (random:uniform *random-state*) 0.2))
      (tested-in-staging . ,(> (random:uniform *random-state*) 0.7))
      (requester . ,(format #f "simulator-~a@changeflow.test"
                            (random 100 *random-state*)))
      (created-at . ,(current-time))
      (time-pressure . ,time-multiplier)
      (simulation . #t))))

;; Generate chaos change (deliberately problematic)
(define (generate-chaos-change scenario)
  (case scenario
    ((friday-afternoon)
     `((change-id . ,(format #f "CHG-CHAOS-FRIDAY-~a" (random 1000 *random-state*)))
       (type . emergency)
       (title . "URGENT: Production hotfix before weekend")
       (risk-level . critical)
       (environment . production)
       (components-affected . 15)
       (has-rollback . #f)  ; No rollback!
       (tested-in-staging . #f)  ; Not tested!
       (created-at . ,(current-time))
       (chaos-type . friday-afternoon-emergency)))

    ((cascade-failure)
     `((change-id . ,(format #f "CHG-CASCADE-~a" (random 1000 *random-state*)))
       (type . normal)
       (title . "Database schema update with dependencies")
       (risk-level . high)
       (environment . production)
       (components-affected . 25)
       (dependencies . ("CHG-DB-001" "CHG-APP-002" "CHG-API-003"))
       (has-rollback . #t)
       (tested-in-staging . #t)
       (created-at . ,(current-time))
       (chaos-type . cascading-dependencies)))

    ((malicious-bypass)
     `((change-id . ,(format #f "CHG-MALICIOUS-~a" (random 1000 *random-state*)))
       (type . standard)  ; Pretending to be standard
       (title . "Minor configuration update")
       (risk-level . low)  ; Lying about risk
       (environment . production)
       (components-affected . 50)  ; Actually affects everything
       (has-rollback . #t)
       (tested-in-staging . #f)  ; Not really tested
       (created-at . ,(current-time))
       (chaos-type . malicious-actor)
       (actual-risk . critical)))

    (else
     (generate-realistic-change))))

;; Generate edge case changes
(define (generate-edge-case edge-type)
  (case edge-type
    ((empty-title)
     `((change-id . "CHG-EDGE-001")
       (type . standard)
       (title . "")  ; Empty title
       (risk-level . low)
       (environment . test)))

    ((huge-description)
     `((change-id . "CHG-EDGE-002")
       (type . normal)
       (title . "Large description test")
       (description . ,(make-string 10000 #\A))  ; 10KB description
       (risk-level . medium)
       (environment . staging)))

    ((future-date)
     `((change-id . "CHG-EDGE-003")
       (type . standard)
       (title . "Time travel test")
       (scheduled-at . ,(add-duration (current-time)
                                       (make-time time-duration 0 86400)))  ; Tomorrow
       (risk-level . low)
       (environment . dev)))

    ((circular-dependency)
     `((change-id . "CHG-EDGE-004")
       (type . normal)
       (title . "Circular dependency test")
       (dependencies . ("CHG-EDGE-005" "CHG-EDGE-006"))
       (risk-level . high)
       (environment . test)))

    (else
     (generate-realistic-change))))

;; Generate batch of changes for load testing
(define* (generate-batch-changes count #:key (chaos-ratio 0.1))
  (let loop ((n count)
             (changes '()))
    (if (= n 0)
        (reverse changes)
        (let ((change (if (< (random:uniform *random-state*) chaos-ratio)
                          (generate-chaos-change
                           (list-ref '(friday-afternoon cascade-failure malicious-bypass)
                                     (random 3 *random-state*)))
                          (generate-realistic-change))))
          (loop (- n 1) (cons change changes))))))

;; Module initialization
(format #t "~%=== Change Generator Module Loaded ===~%")
(format #t "Ready to generate beautiful chaos!~%~%")