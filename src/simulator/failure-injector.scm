;;; failure-injector.scm - Inject realistic failure scenarios
;;; Part of the ITIL Process Simulator - Beautiful Chaos at Scale

(define-module (simulator failure-injector)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-27)
  #:export (inject-failure
            simulate-network-issues
            simulate-resource-constraints
            simulate-dependency-failure
            simulate-human-error
            calculate-failure-probability))

;; Failure type probabilities (based on real-world data)
(define failure-type-weights
  '((network . 0.25)       ; 25% network issues
    (resource . 0.20)      ; 20% resource constraints
    (dependency . 0.15)    ; 15% dependency failures
    (configuration . 0.15) ; 15% config errors
    (human . 0.10)         ; 10% human errors
    (security . 0.05)      ; 5% security blocks
    (unknown . 0.10)))     ; 10% unknown failures

;; Initialize random state
(define *random-state* (random-state-from-platform))

;; Calculate failure probability based on factors
(define (calculate-failure-probability change-data)
  (let* ((base-prob 0.05)  ; 5% base failure rate
         (risk-factor (case (assoc-ref change-data 'risk-level)
                        ((critical) 0.30)
                        ((high) 0.20)
                        ((medium) 0.10)
                        ((low) 0.05)
                        (else 0.05)))
         (env-factor (case (assoc-ref change-data 'environment)
                       ((production) 0.15)
                       ((staging) 0.10)
                       ((test) 0.05)
                       ((dev) 0.02)
                       (else 0.05)))
         (tested-factor (if (assoc-ref change-data 'tested-in-staging)
                            0.0
                            0.15))
         (rollback-factor (if (assoc-ref change-data 'has-rollback)
                              0.0
                              0.10))
         (time-factor (or (assoc-ref change-data 'time-pressure) 1.0)))
    
    ;; Calculate combined probability (capped at 0.95)
    (min 0.95
         (* base-prob
            (+ 1.0 risk-factor env-factor tested-factor rollback-factor)
            time-factor))))

;; Inject a failure based on change characteristics
(define (inject-failure change-data)
  (let ((fail-prob (calculate-failure-probability change-data)))
    (if (< (random:uniform *random-state*) fail-prob)
        (let ((failure-type (weighted-random failure-type-weights)))
          (case failure-type
            ((network) (simulate-network-issues change-data))
            ((resource) (simulate-resource-constraints change-data))
            ((dependency) (simulate-dependency-failure change-data))
            ((configuration) (simulate-configuration-error change-data))
            ((human) (simulate-human-error change-data))
            ((security) (simulate-security-block change-data))
            (else (simulate-unknown-failure change-data))))
        #f)))  ; No failure

;; Network failure scenarios
(define (simulate-network-issues change-data)
  `((failure-type . network)
    (change-id . ,(assoc-ref change-data 'change-id))
    (timestamp . ,(current-time))
    (severity . ,(random-severity))
    (description . "Network connectivity lost during deployment")
    (symptoms . ("Connection timeout" "DNS resolution failed" "Packet loss detected"))
    (impact . ,(format #f "~a components unreachable"
                       (assoc-ref change-data 'components-affected)))
    (recovery-time . ,(+ 300 (random 3600 *random-state*)))  ; 5 min to 1 hour
    (rollback-possible . ,(assoc-ref change-data 'has-rollback))))

;; Resource constraint scenarios
(define (simulate-resource-constraints change-data)
  `((failure-type . resource)
    (change-id . ,(assoc-ref change-data 'change-id))
    (timestamp . ,(current-time))
    (severity . ,(random-severity))
    (description . "Resource exhaustion during deployment")
    (resource-type . ,(list-ref '(cpu memory disk io) (random 4 *random-state*)))
    (utilization . ,(+ 95 (random 5 *random-state*)))  ; 95-100%
    (symptoms . ("Out of memory" "CPU throttling" "Disk full"))
    (impact . "Service degradation or failure")
    (recovery-time . ,(+ 600 (random 1800 *random-state*)))))

;; Dependency failure scenarios
(define (simulate-dependency-failure change-data)
  (let ((deps (or (assoc-ref change-data 'dependencies) '())))
    `((failure-type . dependency)
      (change-id . ,(assoc-ref change-data 'change-id))
      (timestamp . ,(current-time))
      (severity . critical)
      (description . "Dependent service failure")
      (failed-dependency . ,(if (null? deps)
                                 "external-service"
                                 (list-ref deps (random (length deps) *random-state*))))
      (symptoms . ("Service unavailable" "Timeout waiting for dependency"))
      (cascade-risk . high)
      (recovery-time . ,(+ 900 (random 3600 *random-state*))))))

;; Configuration error scenarios
(define (simulate-configuration-error change-data)
  `((failure-type . configuration)
    (change-id . ,(assoc-ref change-data 'change-id))
    (timestamp . ,(current-time))
    (severity . ,(random-severity))
    (description . "Configuration mismatch detected")
    (config-type . ,(list-ref '(database cache load-balancer auth)
                               (random 4 *random-state*)))
    (symptoms . ("Invalid configuration" "Service won't start" "Authentication failures"))
    (detection-time . ,(random 300 *random-state*))  ; How long to detect
    (fix-time . ,(+ 300 (random 900 *random-state*)))))

;; Human error scenarios
(define (simulate-human-error change-data)
  `((failure-type . human)
    (change-id . ,(assoc-ref change-data 'change-id))
    (timestamp . ,(current-time))
    (severity . ,(random-severity))
    (description . "Human error during deployment")
    (error-type . ,(list-ref '(wrong-version wrong-environment missing-step typo)
                              (random 4 *random-state*)))
    (actor . ,(assoc-ref change-data 'requester))
    (symptoms . ("Wrong version deployed" "Deployed to wrong environment" "Step skipped"))
    (discovered-by . ,(list-ref '(monitoring user-report post-deploy-check)
                                 (random 3 *random-state*)))
    (recovery-time . ,(+ 600 (random 2400 *random-state*)))))

;; Security block scenarios
(define (simulate-security-block change-data)
  `((failure-type . security)
    (change-id . ,(assoc-ref change-data 'change-id))
    (timestamp . ,(current-time))
    (severity . critical)
    (description . "Security policy violation detected")
    (violation-type . ,(list-ref '(unauthorized-access policy-violation suspicious-activity)
                                  (random 3 *random-state*)))
    (blocked-by . "security-automation")
    (requires-approval . #t)
    (escalation-required . #t)
    (resolution-time . ,(+ 1800 (random 7200 *random-state*)))))

;; Unknown failure scenarios
(define (simulate-unknown-failure change-data)
  `((failure-type . unknown)
    (change-id . ,(assoc-ref change-data 'change-id))
    (timestamp . ,(current-time))
    (severity . ,(random-severity))
    (description . "Unexpected failure occurred")
    (symptoms . ("Service unresponsive" "Unknown error" "Intermittent failures"))
    (investigation-required . #t)
    (root-cause . pending)
    (recovery-time . ,(+ 1800 (random 10800 *random-state*)))))

;; Helper: Weighted random selection
(define (weighted-random items)
  (let* ((total (apply + (map cdr items)))
         (r (random:uniform *random-state*))
         (target (* r total)))
    (let loop ((items items)
               (sum 0))
      (if (null? items)
          'unknown
          (let ((new-sum (+ sum (cdar items))))
            (if (< target new-sum)
                (caar items)
                (loop (cdr items) new-sum)))))))

;; Helper: Random severity
(define (random-severity)
  (list-ref '(low medium high critical)
            (random 4 *random-state*)))

;; Module initialization
(format #t "~%=== Failure Injector Module Loaded ===~%")
(format #t "Ready to inject beautiful failures!~%~%")