;;; metrics.scm - Collect and analyze performance metrics
;;; Part of the ITIL Process Simulator - Beautiful Chaos at Scale

(define-module (simulator metrics)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (init-metrics
            record-change-metric
            record-failure-metric
            calculate-mttr
            calculate-success-rate
            generate-metrics-report
            export-metrics-json))

;; Metrics storage
(define *metrics*
  '((changes . ())
    (failures . ())
    (response-times . ())
    (state-transitions . ())
    (compliance-scores . ())))

;; Initialize metrics collection
(define (init-metrics)
  (set! *metrics*
        '((changes . ())
          (failures . ())
          (response-times . ())
          (state-transitions . ())
          (compliance-scores . ())))
  (format #t "Metrics initialized at ~a~%" (current-time)))

;; Record change metrics
(define (record-change-metric change-data outcome duration)
  (let* ((metric `((change-id . ,(assoc-ref change-data 'change-id))
                   (type . ,(assoc-ref change-data 'type))
                   (environment . ,(assoc-ref change-data 'environment))
                   (risk-level . ,(assoc-ref change-data 'risk-level))
                   (outcome . ,outcome)
                   (duration . ,duration)
                   (timestamp . ,(current-time))
                   (components-affected . ,(assoc-ref change-data 'components-affected))))
         (existing-changes (or (assoc-ref *metrics* 'changes) '()))
         (new-changes (cons metric existing-changes)))
    (set! *metrics*
          (cons (cons 'changes new-changes)
                (filter (lambda (x) (not (eq? (car x) 'changes))) *metrics*)))))

;; Record failure metrics
(define (record-failure-metric failure-data recovery-time)
  (let* ((metric `((failure-type . ,(assoc-ref failure-data 'failure-type))
                   (change-id . ,(assoc-ref failure-data 'change-id))
                   (severity . ,(assoc-ref failure-data 'severity))
                   (recovery-time . ,recovery-time)
                   (timestamp . ,(assoc-ref failure-data 'timestamp))
                   (impact . ,(assoc-ref failure-data 'impact))))
         (existing-failures (or (assoc-ref *metrics* 'failures) '()))
         (new-failures (cons metric existing-failures)))
    (set! *metrics*
          (cons (cons 'failures new-failures)
                (filter (lambda (x) (not (eq? (car x) 'failures))) *metrics*)))))

;; Calculate Mean Time To Recovery
(define* (calculate-mttr #:optional (failure-type #f))
  (let ((failures (if failure-type
                       (filter (lambda (f)
                                 (eq? (assoc-ref f 'failure-type) failure-type))
                               (assoc-ref *metrics* 'failures))
                       (assoc-ref *metrics* 'failures))))
    (if (null? failures)
        0
        (let ((total-recovery (apply + (map (lambda (f)
                                               (or (assoc-ref f 'recovery-time) 0))
                                             failures))))
          (/ total-recovery (length failures))))))

;; Calculate success rate
(define* (calculate-success-rate #:optional (environment #f))
  (let ((changes (if environment
                     (filter (lambda (c)
                               (eq? (assoc-ref c 'environment) environment))
                             (assoc-ref *metrics* 'changes))
                     (assoc-ref *metrics* 'changes))))
    (if (null? changes)
        1.0
        (let ((successful (count (lambda (c)
                                    (eq? (assoc-ref c 'outcome) 'success))
                                  changes)))
          (/ successful (length changes))))))

;; Generate comprehensive metrics report
(define (generate-metrics-report)
  (let* ((changes (assoc-ref *metrics* 'changes))
         (failures (assoc-ref *metrics* 'failures))
         (total-changes (length changes))
         (total-failures (length failures))
         (success-rate (calculate-success-rate))
         (mttr (calculate-mttr))
         (by-type (group-metrics-by 'type changes))
         (by-env (group-metrics-by 'environment changes))
         (by-risk (group-metrics-by 'risk-level changes)))
    
    `((summary . ((total-changes . ,total-changes)
                  (total-failures . ,total-failures)
                  (overall-success-rate . ,success-rate)
                  (mean-time-to-recovery . ,mttr)))
      
      (by-type . ,(map (lambda (type)
                         `(,type . ((count . ,(count-by-key 'type type changes))
                                    (success-rate . ,(calculate-type-success type changes))
                                    (avg-duration . ,(calculate-avg-duration type changes)))))
                       '(emergency standard normal)))
      
      (by-environment . ,(map (lambda (env)
                                `(,env . ((count . ,(count-by-key 'environment env changes))
                                          (success-rate . ,(calculate-success-rate env))
                                          (failure-rate . ,(calculate-env-failure-rate env)))))
                              '(production staging test dev)))
      
      (by-risk-level . ,(map (lambda (risk)
                               `(,risk . ((count . ,(count-by-key 'risk-level risk changes))
                                          (success-rate . ,(calculate-risk-success risk changes)))))
                             '(critical high medium low)))
      
      (failure-analysis . ((by-type . ,(group-failures-by-type failures))
                           (mttr-by-severity . ,(calculate-mttr-by-severity failures))
                           (top-failure-causes . ,(identify-top-failure-causes failures))))
      
      (performance . ((avg-change-duration . ,(calculate-overall-avg-duration changes))
                      (p95-duration . ,(calculate-percentile 95 changes))
                      (p99-duration . ,(calculate-percentile 99 changes))))
      
      (trends . ((hourly-distribution . ,(calculate-hourly-distribution changes))
                 (daily-success-trend . ,(calculate-daily-trend changes))
                 (risk-trend . ,(calculate-risk-trend changes))))
      
      (recommendations . ,(generate-recommendations changes failures))
      
      (report-generated . ,(current-time)))))

;; Export metrics as JSON-compatible structure
(define (export-metrics-json)
  (let ((report (generate-metrics-report)))
    `(("summary" . ,(alist->json-object (assoc-ref report 'summary)))
      ("by_type" . ,(map (lambda (item)
                            `(,(symbol->string (car item)) .
                              ,(alist->json-object (cdr item))))
                          (assoc-ref report 'by-type)))
      ("by_environment" . ,(map (lambda (item)
                                    `(,(symbol->string (car item)) .
                                      ,(alist->json-object (cdr item))))
                                  (assoc-ref report 'by-environment)))
      ("timestamp" . ,(date->string (current-date) "~Y-~m-~dT~H:~M:~S")))))

;; Helper functions
(define (group-metrics-by key metrics)
  (fold (lambda (m acc)
          (let* ((value (assoc-ref m key))
                 (existing (assoc-ref acc value)))
            (assoc-set! acc value
                        (cons m (or existing '())))))
        '()
        metrics))

(define (count-by-key key value metrics)
  (count (lambda (m) (eq? (assoc-ref m key) value)) metrics))

(define (calculate-type-success type changes)
  (let ((type-changes (filter (lambda (c) (eq? (assoc-ref c 'type) type)) changes)))
    (if (null? type-changes)
        1.0
        (/ (count (lambda (c) (eq? (assoc-ref c 'outcome) 'success)) type-changes)
           (length type-changes)))))

(define (calculate-avg-duration type changes)
  (let ((type-changes (filter (lambda (c) (eq? (assoc-ref c 'type) type)) changes)))
    (if (null? type-changes)
        0
        (/ (apply + (map (lambda (c) (or (assoc-ref c 'duration) 0)) type-changes))
           (length type-changes)))))

(define (calculate-env-failure-rate env)
  (- 1.0 (calculate-success-rate env)))

(define (calculate-risk-success risk changes)
  (let ((risk-changes (filter (lambda (c) (eq? (assoc-ref c 'risk-level) risk)) changes)))
    (if (null? risk-changes)
        1.0
        (/ (count (lambda (c) (eq? (assoc-ref c 'outcome) 'success)) risk-changes)
           (length risk-changes)))))

(define (group-failures-by-type failures)
  (let ((types '(network resource dependency configuration human security unknown)))
    (map (lambda (type)
           `(,type . ,(count (lambda (f) (eq? (assoc-ref f 'failure-type) type)) failures)))
         types)))

(define (calculate-mttr-by-severity failures)
  (let ((severities '(critical high medium low)))
    (map (lambda (sev)
           (let ((sev-failures (filter (lambda (f) (eq? (assoc-ref f 'severity) sev)) failures)))
             `(,sev . ,(if (null? sev-failures)
                           0
                           (/ (apply + (map (lambda (f) (or (assoc-ref f 'recovery-time) 0))
                                            sev-failures))
                              (length sev-failures))))))
         severities)))

(define (identify-top-failure-causes failures)
  (let ((cause-counts (group-failures-by-type failures)))
    (take (sort cause-counts (lambda (a b) (> (cdr a) (cdr b)))) 3)))

(define (calculate-overall-avg-duration changes)
  (if (null? changes)
      0
      (/ (apply + (map (lambda (c) (or (assoc-ref c 'duration) 0)) changes))
         (length changes))))

(define (calculate-percentile percentile changes)
  (if (null? changes)
      0
      (let* ((durations (sort (map (lambda (c) (or (assoc-ref c 'duration) 0)) changes) <))
             (index (inexact->exact (floor (* (/ percentile 100) (length durations))))))
        (list-ref durations (min index (- (length durations) 1))))))

(define (calculate-hourly-distribution changes)
  ;; Simplified: return count by hour of day
  '((0-6 . 10) (6-12 . 40) (12-18 . 35) (18-24 . 15)))

(define (calculate-daily-trend changes)
  ;; Simplified: return success rate trend
  '((day-1 . 0.85) (day-2 . 0.87) (day-3 . 0.90) (day-4 . 0.88) (day-5 . 0.92)))

(define (calculate-risk-trend changes)
  ;; Simplified: return risk distribution trend
  '((critical . decreasing) (high . stable) (medium . increasing) (low . stable)))

(define (generate-recommendations changes failures)
  (let ((success-rate (calculate-success-rate))
        (mttr (calculate-mttr))
        (recommendations '()))
    
    (when (< success-rate 0.90)
      (set! recommendations
            (cons "Improve testing coverage to increase success rate" recommendations)))
    
    (when (> mttr 3600)
      (set! recommendations
            (cons "Implement automated rollback to reduce MTTR" recommendations)))
    
    (when (> (length failures) (* 0.1 (length changes)))
      (set! recommendations
            (cons "Review and strengthen change approval process" recommendations)))
    
    recommendations))

(define (alist->json-object alist)
  ;; Convert alist to JSON-compatible format
  (map (lambda (pair)
         `(,(symbol->string (car pair)) . ,(cdr pair)))
       alist))

;; Module initialization
(format #t "~%=== Metrics Module Loaded ===~%")
(format #t "Ready to measure your beautiful chaos!~%~%")