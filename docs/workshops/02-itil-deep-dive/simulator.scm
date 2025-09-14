#!/usr/bin/env guile
!#
;;; ITIL Change Management Simulator
;;; Workshop 02 - Hands-on Lab Exercise
;;; 
;;; This simulator demonstrates modern change management principles
;;; by processing changes through risk assessment and approval workflows

(use-modules (srfi srfi-9)     ; Record types
             (srfi srfi-27)    ; Random numbers
             (srfi srfi-19)    ; Time/date
             (ice-9 format))   ; Formatted output

;; Simple change request record (without external dependencies)
(define-record-type <change-request>
  (make-change-request id title description risk-score status created-at updated-at)
  change-request?
  (id change-request-id)
  (title change-request-title)
  (description change-request-description)
  (risk-score change-request-risk-score set-change-request-risk-score!)
  (status change-request-status set-change-request-status!)
  (created-at change-request-created-at)
  (updated-at change-request-updated-at set-change-request-updated-at!))

;; Initialize random seed
(random-source-randomize! default-random-source)

;; Change type distribution (realistic per Atlassian guide)
(define (generate-change-type)
  (let ((rand (random 100)))
    (cond
      ((< rand 70) 'standard)   ; 70% standard changes
      ((< rand 95) 'normal)     ; 25% normal changes  
      (else 'emergency))))       ; 5% emergency changes

;; Generate realistic risk scores based on change type
(define (generate-risk-score type)
  (case type
    ((standard) (+ 5 (random 25)))      ; 5-30 risk score
    ((normal) (+ 30 (random 40)))       ; 30-70 risk score
    ((emergency) (+ 70 (random 30)))))  ; 70-100 risk score

;; Simulate different times of day/week for changes
(define (generate-timing-context)
  (let ((hour (random 24))
        (day (random 7)))
    (cond
      ;; Friday afternoon - higher risk
      ((and (= day 4) (>= hour 14)) 'friday-afternoon)
      ;; Weekend - mixed (emergency likely)
      ((or (= day 5) (= day 6)) 'weekend)
      ;; Monday morning - high volume
      ((and (= day 0) (<= hour 12)) 'monday-morning)
      ;; Business hours - normal
      ((and (>= hour 9) (<= hour 17)) 'business-hours)
      ;; After hours - emergencies more likely
      (else 'after-hours))))

;; Generate realistic change request with context
(define (generate-change id)
  (let* ((type (generate-change-type))
         (timing (generate-timing-context))
         (base-risk (generate-risk-score type))
         ;; Adjust risk based on timing
         (risk (min 100 (case timing
                         ((friday-afternoon) (inexact->exact (ceiling (* base-risk 1.3))))
                         ((weekend) (if (eq? type 'emergency) 
                                      (+ base-risk 10)
                                      base-risk))
                         (else base-risk))))
         (title (format #f "~a change #~a (~a)" 
                       (string-upcase (symbol->string type))
                       id 
                       timing))
         (description (format #f "Simulated ~a change with risk score ~a"
                            type risk)))
    (make-change-request 
     id title description risk 'submitted
     (current-time) (current-time))))

;; Simulate approval based on risk and type
(define (simulate-approval change)
  (let ((risk (change-request-risk-score change)))
    (cond
      ;; Standard changes - automatic approval
      ((< risk 30)
       (set-change-request-status! change 'auto-approved)
       'auto-approved)
      
      ;; Low-medium risk - peer review
      ((< risk 50)
       (set-change-request-status! change 'peer-review)
       (if (> (random-real) 0.05)  ; 95% approval rate
           (begin
             (set-change-request-status! change 'approved)
             'peer-approved)
           (begin
             (set-change-request-status! change 'rejected)
             'peer-rejected)))
      
      ;; Medium-high risk - team lead approval
      ((< risk 70)
       (set-change-request-status! change 'lead-review)
       (if (> (random-real) 0.15)  ; 85% approval rate
           (begin
             (set-change-request-status! change 'approved)
             'lead-approved)
           (begin
             (set-change-request-status! change 'rejected)
             'lead-rejected)))
      
      ;; High risk - CAB review
      (else
       (set-change-request-status! change 'cab-review)
       (if (> (random-real) 0.25)  ; 75% approval rate for high-risk
           (begin
             (set-change-request-status! change 'emergency-approved)
             'cab-approved)
           (begin
             (set-change-request-status! change 'rejected)
             'cab-rejected))))))

;; Simulate deployment and potential failure
(define (simulate-deployment change)
  (if (member (change-request-status change) 
              '(auto-approved approved peer-approved 
                lead-approved emergency-approved cab-approved))
      (let ((risk (change-request-risk-score change)))
        ;; Higher risk = higher chance of deployment failure
        (if (< (random 100) (- 100 (/ risk 2)))
            (begin
              (set-change-request-status! change 'deployed)
              'deployed-success)
            (begin
              (set-change-request-status! change 'failed)
              'deployed-failed)))
      'not-deployed))

;; Simulate rollback for failed deployments
(define (simulate-rollback change)
  (if (eq? (change-request-status change) 'failed)
      (if (> (random-real) 0.1)  ; 90% successful rollback
          (begin
            (set-change-request-status! change 'rolled-back)
            'rollback-success)
          (begin
            (set-change-request-status! change 'rollback-failed)
            'rollback-failed))
      'no-rollback-needed))

;; Process change through complete workflow
(define (process-change change)
  (let* ((approval-result (simulate-approval change))
         (deployment-result (simulate-deployment change))
         (rollback-result (if (eq? deployment-result 'deployed-failed)
                            (simulate-rollback change)
                            'no-rollback-needed)))
    (list approval-result deployment-result rollback-result)))

;; Calculate metrics from results
(define (calculate-metrics results)
  (let ((total (hash-fold (lambda (k v acc) (+ acc v)) 0 results))
        (auto-approved 0)
        (deployed 0)
        (failed 0)
        (rolled-back 0))
    
    (hash-for-each
     (lambda (key value)
       (when (memq 'auto-approved key) 
         (set! auto-approved (+ auto-approved value)))
       (when (memq 'deployed-success key) 
         (set! deployed (+ deployed value)))
       (when (memq 'deployed-failed key) 
         (set! failed (+ failed value)))
       (when (memq 'rollback-success key) 
         (set! rolled-back (+ rolled-back value))))
     results)
    
    (list
     (cons 'automation-rate (* 100.0 (/ auto-approved total)))
     (cons 'deployment-success-rate (* 100.0 (/ deployed (+ deployed failed))))
     (cons 'rollback-success-rate (if (> failed 0)
                                     (* 100.0 (/ rolled-back failed))
                                     100.0)))))

;; Run simulation with detailed results
(define (run-simulation num-changes)
  (let ((results (make-hash-table))
        (risk-totals (make-hash-table))
        (type-counts (make-hash-table)))
    
    ;; Process all changes
    (do ((i 1 (+ i 1)))
        ((> i num-changes))
      (let* ((change (generate-change i))
             (risk (change-request-risk-score change))
             (result (process-change change)))
        
        ;; Track results
        (hash-set! results result 
                  (+ 1 (or (hash-ref results result) 0)))
        
        ;; Track risk categories
        (let ((risk-category (cond
                             ((< risk 30) 'low-risk)
                             ((< risk 70) 'medium-risk)
                             (else 'high-risk))))
          (hash-set! risk-totals risk-category
                    (+ 1 (or (hash-ref risk-totals risk-category) 0))))))
    
    ;; Display results with beautiful formatting
    (format #t "\n╔════════════════════════════════════════════════╗\n")
    (format #t "║     ITIL CHANGE SIMULATION RESULTS (~3d)       ║\n" num-changes)
    (format #t "╠════════════════════════════════════════════════╣\n")
    (format #t "║                                                ║\n")
    (format #t "║  Risk Distribution:                            ║\n")
    
    (hash-for-each
     (lambda (key value)
       (let ((percentage (* 100.0 (/ value num-changes))))
         (format #t "║    ~12a: ~3d (~5,1f%)               ║\n" 
                 key value percentage)))
     risk-totals)
    
    (format #t "║                                                ║\n")
    (format #t "║  Approval Outcomes:                            ║\n")
    
    (let ((approval-stats (make-hash-table)))
      (hash-for-each
       (lambda (key value)
         (let ((approval-type (car key)))
           (hash-set! approval-stats approval-type
                     (+ value (or (hash-ref approval-stats approval-type) 0)))))
       results)
      
      (hash-for-each
       (lambda (key value)
         (when (string-suffix? "approved" (symbol->string key))
           (format #t "║    ~15a: ~3d (~5,1f%)          ║\n"
                   key value (* 100.0 (/ value num-changes)))))
       approval-stats))
    
    ;; Calculate and display metrics
    (let ((metrics (calculate-metrics results)))
      (format #t "║                                                ║\n")
      (format #t "║  Key Performance Metrics:                      ║\n")
      (for-each 
       (lambda (metric)
         (format #t "║    ~25a: ~5,1f%      ║\n"
                 (car metric) (cdr metric)))
       metrics))
    
    (format #t "╚════════════════════════════════════════════════╝\n\n")))

;; Display usage information
(define (show-usage)
  (format #t "
ITIL Change Management Simulator
=================================

Usage: ~a [number-of-changes]

Examples:
  ~a 100     # Simulate 100 changes
  ~a 1000    # Simulate 1000 changes

This simulator demonstrates modern ITIL change management principles:
- 70% standard changes (auto-approved)
- Risk-based approval routing
- Realistic failure and rollback scenarios
- Time-of-day risk adjustments

Based on Workshop 02: ITIL Change Management Deep Dive
" 
          (car (command-line))
          (car (command-line))
          (car (command-line))))

;; Main entry point
(define (main args)
  (cond
    ((= (length args) 1)
     (show-usage))
    ((= (length args) 2)
     (let ((num-changes (string->number (cadr args))))
       (if (and num-changes (> num-changes 0))
           (run-simulation num-changes)
           (begin
             (format #t "Error: Please provide a positive number of changes\n")
             (show-usage)))))
    (else
     (show-usage))))

;; Run main if executed as script
(main (command-line))