;;; state-validator.scm - Validate state transitions and audit compliance
;;; Part of the ITIL Process Simulator - Beautiful Chaos at Scale

(define-module (simulator state-validator)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (validate-state-transition
            validate-audit-trail
            check-compliance
            detect-violations
            generate-compliance-report))

;; Valid state transitions for ITIL changes
(define valid-transitions
  '((draft . (submitted cancelled))
    (submitted . (approved rejected cancelled))
    (approved . (scheduled cancelled))
    (scheduled . (in-progress cancelled postponed))
    (in-progress . (completed failed rolled-back))
    (completed . (closed))
    (failed . (rolled-back retrying closed))
    (rolled-back . (closed retrying))
    (cancelled . (closed))
    (closed . ())))

;; Required fields by change type
(define required-fields
  '((emergency . (change-id type title justification impact requester priority))
    (standard . (change-id type title description risk-level environment))
    (normal . (change-id type title description risk-level environment
               tested-in-staging has-rollback cab-approval))))

;; Compliance rules
(define compliance-rules
  '((emergency-response-time . 30)    ; 30 minutes max
    (standard-lead-time . 1440)       ; 24 hours minimum
    (normal-lead-time . 4320)         ; 72 hours minimum
    (cab-approval-required . (normal emergency))
    (testing-required . (normal))
    (rollback-required . (normal emergency))
    (audit-retention-days . 90)))

;; Validate state transition
(define (validate-state-transition from-state to-state change-type)
  (let ((allowed (assoc-ref valid-transitions from-state)))
    (if (or (not allowed)
            (not (member to-state allowed)))
        `((valid . #f)
          (from . ,from-state)
          (to . ,to-state)
          (reason . ,(format #f "Invalid transition: ~a -> ~a"
                             from-state to-state))
          (allowed-transitions . ,allowed)
          (violation-type . invalid-state-transition))
        `((valid . #t)
          (from . ,from-state)
          (to . ,to-state)
          (transition-time . ,(current-time))))))

;; Validate audit trail completeness
(define (validate-audit-trail audit-entries change-data)
  (let* ((required-events '(created submitted approved scheduled
                            started completed closed))
         (actual-events (map (lambda (e) (assoc-ref e 'event)) audit-entries))
         (missing-events (filter (lambda (e) (not (member e actual-events)))
                                 required-events))
         (chronological? (check-chronological-order audit-entries))
         (has-actors? (all-have-actors? audit-entries)))
    
    `((complete . ,(null? missing-events))
      (chronological . ,chronological?)
      (has-actors . ,has-actors?)
      (missing-events . ,missing-events)
      (total-events . ,(length audit-entries))
      (validation-time . ,(current-time))
      (issues . ,(append
                  (if (not (null? missing-events))
                      `((missing-audit-events . ,missing-events))
                      '())
                  (if (not chronological?)
                      '((audit-order . "Events not in chronological order"))
                      '())
                  (if (not has-actors?)
                      '((missing-actors . "Some events lack actor information"))
                      '()))))))

;; Check ITIL compliance
(define (check-compliance change-data audit-trail)
  (let* ((change-type (assoc-ref change-data 'change-id))
         (violations '())
         (warnings '()))
    
    ;; Check required fields
    (let ((required (assoc-ref required-fields
                               (assoc-ref change-data 'type))))
      (when required
        (for-each (lambda (field)
                    (when (not (assoc-ref change-data field))
                      (set! violations
                            (cons `(missing-field . ,field) violations))))
                  required)))
    
    ;; Check lead time compliance
    (let* ((created-time (find-event-time 'created audit-trail))
           (started-time (find-event-time 'started audit-trail))
           (lead-time (if (and created-time started-time)
                          (time-difference started-time created-time)
                          #f)))
      (when lead-time
        (let ((min-lead (case (assoc-ref change-data 'type)
                          ((emergency) 0)
                          ((standard) 1440)
                          ((normal) 4320)
                          (else 0))))
          (when (< (time-second lead-time) (* min-lead 60))
            (set! violations
                  (cons `(insufficient-lead-time . ,lead-time) violations))))))
    
    ;; Check CAB approval
    (when (member (assoc-ref change-data 'type) '(normal emergency))
      (when (not (find-event 'cab-approved audit-trail))
        (set! warnings
              (cons '(missing-cab-approval . "CAB approval not recorded")
                    warnings))))
    
    ;; Check testing requirements
    (when (eq? (assoc-ref change-data 'type) 'normal)
      (when (not (assoc-ref change-data 'tested-in-staging))
        (set! warnings
              (cons '(not-tested . "Change not tested in staging")
                    warnings))))
    
    ;; Return compliance result
    `((compliant . ,(null? violations))
      (violations . ,violations)
      (warnings . ,warnings)
      (checked-at . ,(current-time))
      (change-type . ,(assoc-ref change-data 'type)))))

;; Detect policy violations
(define (detect-violations change-data audit-trail)
  (let ((violations '()))
    
    ;; Check for unauthorized changes
    (when (and (eq? (assoc-ref change-data 'environment) 'production)
               (not (assoc-ref change-data 'approved-by)))
      (set! violations
            (cons '(unauthorized . "Production change without approval")
                  violations)))
    
    ;; Check for freeze period violations
    (when (in-freeze-period? (current-time))
      (when (not (eq? (assoc-ref change-data 'type) 'emergency))
        (set! violations
              (cons '(freeze-violation . "Non-emergency change during freeze")
                    violations))))
    
    ;; Check for excessive risk
    (when (and (eq? (assoc-ref change-data 'risk-level) 'critical)
               (not (assoc-ref change-data 'risk-mitigation)))
      (set! violations
            (cons '(unmitigated-risk . "Critical risk without mitigation plan")
                  violations)))
    
    ;; Check for missing rollback
    (when (and (member (assoc-ref change-data 'environment) '(production staging))
               (not (assoc-ref change-data 'has-rollback)))
      (set! violations
            (cons '(no-rollback . "No rollback plan for critical environment")
                  violations)))
    
    violations))

;; Generate compliance report
(define (generate-compliance-report changes audit-trails)
  (let* ((total-changes (length changes))
         (compliance-results (map (lambda (c a)
                                     (check-compliance c a))
                                   changes audit-trails))
         (compliant-count (count (lambda (r)
                                    (assoc-ref r 'compliant))
                                 compliance-results))
         (violation-summary (aggregate-violations compliance-results)))
    
    `((report-date . ,(current-time))
      (total-changes . ,total-changes)
      (compliant-changes . ,compliant-count)
      (compliance-rate . ,(/ compliant-count total-changes))
      (violations-by-type . ,violation-summary)
      (recommendations . ,(generate-recommendations violation-summary))
      (risk-level . ,(calculate-overall-risk violation-summary)))))

;; Helper functions
(define (check-chronological-order entries)
  (or (< (length entries) 2)
      (let loop ((entries entries))
        (if (< (length entries) 2)
            #t
            (let ((t1 (assoc-ref (car entries) 'timestamp))
                  (t2 (assoc-ref (cadr entries) 'timestamp)))
              (and (time<=? t1 t2)
                   (loop (cdr entries))))))))

(define (all-have-actors? entries)
  (every (lambda (e) (assoc-ref e 'actor)) entries))

(define (find-event event-type audit-trail)
  (find (lambda (e) (eq? (assoc-ref e 'event) event-type))
        audit-trail))

(define (find-event-time event-type audit-trail)
  (let ((event (find-event event-type audit-trail)))
    (and event (assoc-ref event 'timestamp))))

(define (in-freeze-period? time)
  ;; Simplified: freeze on weekends
  (let ((day (date-week-day (time-utc->date time))))
    (or (= day 0) (= day 6))))  ; Sunday or Saturday

(define (aggregate-violations results)
  (let ((all-violations (append-map (lambda (r)
                                       (assoc-ref r 'violations))
                                    results)))
    (fold (lambda (v acc)
            (let* ((type (car v))
                   (count (assoc-ref acc type)))
              (assoc-set! acc type (+ (or count 0) 1))))
          '()
          all-violations)))

(define (generate-recommendations violations)
  (let ((recs '()))
    (when (assoc-ref violations 'missing-field)
      (set! recs (cons "Ensure all required fields are populated" recs)))
    (when (assoc-ref violations 'insufficient-lead-time)
      (set! recs (cons "Plan changes with adequate lead time" recs)))
    (when (assoc-ref violations 'missing-cab-approval)
      (set! recs (cons "Implement CAB approval workflow" recs)))
    (when (assoc-ref violations 'not-tested)
      (set! recs (cons "Mandate staging environment testing" recs)))
    recs))

(define (calculate-overall-risk violations)
  (cond
    ((> (length violations) 5) 'critical)
    ((> (length violations) 3) 'high)
    ((> (length violations) 1) 'medium)
    ((> (length violations) 0) 'low)
    (else 'minimal)))

;; Module initialization
(format #t "~%=== State Validator Module Loaded ===~%")
(format #t "Ready to validate your beautiful chaos!~%~%")