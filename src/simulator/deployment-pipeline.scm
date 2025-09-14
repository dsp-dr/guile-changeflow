;;; deployment-pipeline.scm - Realistic deployment pipeline simulator
;;; Simulates 100 PRs across frontend, backend, IaC components

(define-module (simulator deployment-pipeline)
  #:use-module (simulator change-generator)
  #:use-module (simulator failure-injector)
  #:use-module (simulator state-validator)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-27)
  #:export (generate-mock-prs
            simulate-deployment-pipeline
            process-deployment-queue
            demo-20-deployments))

;; Component types and their characteristics
(define component-types
  '((frontend . ((count . 30)
                 (deploy-time . (300 900))    ; 5-15 min
                 (failure-rate . 0.08)
                 (requires-migration . #f)
                 (rollback-time . 120)))
    
    (backend-api . ((count . 25)
                    (deploy-time . (600 1800))  ; 10-30 min
                    (failure-rate . 0.12)
                    (requires-migration . #f)
                    (rollback-time . 300)))
    
    (backend-db . ((count . 15)
                   (deploy-time . (900 2700))   ; 15-45 min
                   (failure-rate . 0.20)
                   (requires-migration . #t)
                   (rollback-time . 1800)))     ; 30 min rollback
    
    (iac-app . ((count . 20)
                (deploy-time . (1200 3600))     ; 20-60 min
                (failure-rate . 0.15)
                (requires-migration . #f)
                (rollback-time . 600)))
    
    (iac-global . ((count . 10)
                   (deploy-time . (1800 5400))  ; 30-90 min
                   (failure-rate . 0.25)
                   (requires-migration . #f)
                   (rollback-time . 900)))))

;; Global deployment state
(define *deployment-queue* '())
(define *staging-owner* #f)
(define *active-deployments* '())
(define *completed-deployments* '())
(define *pr-counter* 1000)

;; Initialize random state
(define *random-state* (random-state-from-platform))

;; Generate mock PRs across all components
(define (generate-mock-prs)
  "Generate 100 mock PRs across different component types"
  (format #t "🔧 Generating 100 mock PRs across components...~%~%")
  
  (let ((all-prs '()))
    (for-each
     (lambda (component)
       (let* ((comp-name (car component))
              (comp-info (cdr component))
              (count (assoc-ref comp-info 'count))
              (component-prs (generate-component-prs comp-name count)))
         
         (format #t "📦 ~a: ~a PRs~%" comp-name count)
         (set! all-prs (append all-prs component-prs))))
     component-types)
    
    (format #t "~%📊 Total PRs generated: ~a~%~%" (length all-prs))
    all-prs))

;; Generate PRs for specific component type
(define (generate-component-prs component-type count)
  "Generate mock PRs for a specific component"
  (let loop ((n count) (prs '()))
    (if (= n 0)
        prs
        (let ((pr (generate-mock-pr component-type)))
          (loop (- n 1) (cons pr prs))))))

;; Generate individual mock PR
(define (generate-mock-pr component-type)
  "Generate a single mock PR"
  (let* ((pr-number (begin (set! *pr-counter* (+ *pr-counter* 1)) *pr-counter*))
         (comp-info (assoc-ref component-types component-type))
         (deploy-range (assoc-ref comp-info 'deploy-time))
         (deploy-time (+ (car deploy-range)
                         (random (- (cadr deploy-range) (car deploy-range))
                                 *random-state*)))
         (failure-rate (assoc-ref comp-info 'failure-rate))
         (requires-migration (assoc-ref comp-info 'requires-migration))
         (change-type (if requires-migration 'normal 'standard))
         (priority (case component-type
                     ((iac-global) 'high)
                     ((backend-db) 'high)
                     (else 'medium))))
    
    `((pr-number . ,pr-number)
      (component . ,component-type)
      (title . ,(generate-pr-title component-type pr-number))
      (author . ,(generate-mock-author))
      (change-type . ,change-type)
      (priority . ,priority)
      (estimated-deploy-time . ,deploy-time)
      (failure-rate . ,failure-rate)
      (requires-migration . ,requires-migration)
      (created-at . ,(current-time))
      (status . pending)
      (change-id . ,(format #f "CHG-~a-~a" component-type pr-number)))))

;; Generate realistic PR titles
(define (generate-pr-title component-type pr-number)
  "Generate realistic PR titles based on component type"
  (let ((titles
         (case component-type
           ((frontend)
            '("Add dark mode toggle to settings"
              "Fix responsive layout on mobile"
              "Update user profile component"
              "Add loading states to checkout"
              "Implement feature flag for new dashboard"))
           
           ((backend-api)
            '("Add rate limiting to auth endpoints"
              "Update user validation logic"
              "Fix pagination in search API"
              "Add health check endpoint"
              "Optimize database queries"))
           
           ((backend-db)
            '("Add index for user_events table"
              "Migrate legacy user schema"
              "Add audit logging tables"
              "Update foreign key constraints"
              "Partition large events table"))
           
           ((iac-app)
            '("Update ECS task definitions"
              "Add auto-scaling policies"
              "Configure Redis cluster"
              "Update load balancer rules"
              "Add monitoring dashboards"))
           
           ((iac-global)
            '("Add new AWS region support"
              "Update WAF rules for new threats"
              "Configure cross-region replication"
              "Add telemetry pipeline"
              "Update SSL certificates")))))
    
    (format #f "~a (#~a)" 
            (list-ref titles (random (length titles) *random-state*))
            pr-number)))

;; Generate mock author names
(define (generate-mock-author)
  "Generate mock GitHub usernames"
  (let ((authors '("alice-dev" "bob-backend" "charlie-ops" "diana-frontend"
                   "eve-security" "frank-data" "grace-mobile" "henry-infra")))
    (list-ref authors (random (length authors) *random-state*))))

;; Simulate deployment pipeline
(define (simulate-deployment-pipeline prs num-to-process)
  "Simulate realistic deployment pipeline with blocking staging"
  (format #t "🚀 Starting Deployment Pipeline Simulation~%")
  (format #t "Processing ~a PRs from ~a total~%~%" num-to-process (length prs))
  
  ;; Initialize queue with first batch
  (let ((prs-to-process (take prs num-to-process)))
    (set! *deployment-queue* (sort prs-to-process pr-priority-compare))
    (set! *staging-owner* #f)
    (set! *active-deployments* '())
    (set! *completed-deployments* '())
    
    (format #t "📋 Deployment Queue initialized with ~a PRs~%" 
            (length *deployment-queue*))
    
    ;; Process the queue
    (process-deployment-queue)))

;; Process deployment queue with blocking staging
(define (process-deployment-queue)
  "Process deployment queue with realistic staging blocking"
  (let ((current-time (current-time))
        (deployments-processed 0))
    
    (while (or (not (null? *deployment-queue*))
               (not (null? *active-deployments*)))
      
      ;; Check for completed deployments
      (check-completed-deployments current-time)
      
      ;; Start new deployment if staging is free
      (when (and (not (null? *deployment-queue*))
                 (not *staging-owner*))
        (let ((next-pr (car *deployment-queue*)))
          (set! *deployment-queue* (cdr *deployment-queue*))
          (start-deployment next-pr current-time)
          (set! deployments-processed (+ deployments-processed 1))))
      
      ;; Advance time by 5 minutes
      (set! current-time (add-duration current-time (make-time time-duration 0 300)))
      
      ;; Progress update every 10 deployments
      (when (and (> deployments-processed 0)
                 (= (modulo deployments-processed 5) 0))
        (format #t "⏱️  Progress: ~a deployments started, ~a completed~%"
                deployments-processed (length *completed-deployments*))))
    
    (format #t "~%✅ Pipeline simulation complete!~%")
    (format #t "📊 Final stats: ~a deployments processed~%" 
            (length *completed-deployments*))
    
    ;; Return summary
    (generate-pipeline-summary)))

;; Start a new deployment
(define (start-deployment pr current-time)
  "Start deployment for a PR"
  (let* ((pr-num (assoc-ref pr 'pr-number))
         (component (assoc-ref pr 'component))
         (deploy-time (assoc-ref pr 'estimated-deploy-time))
         (end-time (add-duration current-time 
                                 (make-time time-duration 0 deploy-time)))
         (window-end (add-duration end-time
                                   (make-time time-duration 0 3600))) ; +1h buffer
         
         ;; Generate ITIL change request
         (change-request (pr-to-change-request pr))
         
         ;; Determine if deployment will fail
         (will-fail (< (random:uniform *random-state*)
                       (assoc-ref pr 'failure-rate)))
         
         (deployment `((pr . ,pr)
                       (change-request . ,change-request)
                       (start-time . ,current-time)
                       (end-time . ,end-time)
                       (window-end . ,window-end)
                       (will-fail . ,will-fail)
                       (status . deploying))))
    
    (format #t "🔧 [~a] Starting deployment: ~a (~a)~%"
            (time->iso8601-string current-time)
            (assoc-ref pr 'title)
            component)
    
    (format #t "   📋 Change Request: ~a~%" (assoc-ref change-request 'change-id))
    (format #t "   ⏰ Deploy window: ~a minutes (ends ~a)~%"
            (/ deploy-time 60)
            (time->iso8601-string window-end))
    
    ;; Mark staging as owned
    (set! *staging-owner* pr-num)
    (set! *active-deployments* (cons deployment *active-deployments*))))

;; Check for completed deployments
(define (check-completed-deployments current-time)
  "Check if any active deployments have completed"
  (let ((completed '())
        (still-active '()))
    
    (for-each
     (lambda (deployment)
       (let ((end-time (assoc-ref deployment 'end-time))
             (pr (assoc-ref deployment 'pr))
             (will-fail (assoc-ref deployment 'will-fail)))
         
         (if (time>=? current-time end-time)
             ;; Deployment completed
             (let* ((success (not will-fail))
                    (pr-num (assoc-ref pr 'pr-number))
                    (component (assoc-ref pr 'component))
                    (updated-deployment (assoc-set! deployment 'status 
                                                    (if success 'success 'failed))))
               
               (format #t "~a [~a] Deployment ~a: ~a (~a)~%"
                       (if success "✅" "❌")
                       (time->iso8601-string current-time)
                       (if success "succeeded" "failed")
                       (assoc-ref pr 'title)
                       component)
               
               (when (not success)
                 (format #t "   🔄 Initiating rollback procedure...~%"))
               
               ;; Release staging
               (when (= *staging-owner* pr-num)
                 (set! *staging-owner* #f)
                 (format #t "   🆓 Staging environment released~%"))
               
               (set! completed (cons updated-deployment completed)))
             
             ;; Still deploying
             (set! still-active (cons deployment still-active)))))
     *active-deployments*)
    
    ;; Update global state
    (set! *completed-deployments* (append completed *completed-deployments*))
    (set! *active-deployments* still-active)))

;; Convert PR to ITIL change request
(define (pr-to-change-request pr)
  "Convert PR to ITIL change request"
  (let* ((pr-num (assoc-ref pr 'pr-number))
         (component (assoc-ref pr 'component))
         (change-id (assoc-ref pr 'change-id))
         (change-type (assoc-ref pr 'change-type))
         (priority (assoc-ref pr 'priority)))
    
    `((change-id . ,change-id)
      (type . ,change-type)
      (title . ,(assoc-ref pr 'title))
      (description . ,(format #f "Deploy PR #~a to ~a environment" pr-num component))
      (priority . ,priority)
      (requester . ,(assoc-ref pr 'author))
      (component . ,component)
      (pr-reference . ,pr-num)
      (risk-level . ,(case component
                       ((iac-global backend-db) 'high)
                       ((iac-app backend-api) 'medium)
                       (else 'low)))
      (environment . staging)
      (created-at . ,(current-time))
      (status . submitted))))

;; Priority comparison for queue sorting
(define (pr-priority-compare pr1 pr2)
  "Compare PRs for priority queue sorting"
  (let ((p1 (case (assoc-ref pr1 'priority)
              ((high) 3) ((medium) 2) ((low) 1)))
        (p2 (case (assoc-ref pr2 'priority)
              ((high) 3) ((medium) 2) ((low) 1))))
    (> p1 p2)))

;; Generate pipeline summary
(define (generate-pipeline-summary)
  "Generate comprehensive pipeline summary"
  (let* ((total (length *completed-deployments*))
         (successful (count (lambda (d) (eq? (assoc-ref d 'status) 'success))
                           *completed-deployments*))
         (failed (- total successful))
         (success-rate (if (> total 0) (/ successful total) 0)))
    
    (format #t "~%📊 DEPLOYMENT PIPELINE SUMMARY~%")
    (format #t "================================~%")
    (format #t "Total deployments: ~a~%" total)
    (format #t "Successful: ~a (~,1f%)~%" successful (* 100 success-rate))
    (format #t "Failed: ~a (~,1f%)~%" failed (* 100 (- 1 success-rate)))
    
    ;; Component breakdown
    (format #t "~%📦 Component Breakdown:~%")
    (let ((by-component (group-by-component *completed-deployments*)))
      (for-each
       (lambda (comp-group)
         (let* ((comp (car comp-group))
                (deployments (cdr comp-group))
                (comp-success (count (lambda (d) (eq? (assoc-ref d 'status) 'success))
                                    deployments)))
           (format #t "  ~a: ~a deployments (~a successful)~%"
                   comp (length deployments) comp-success)))
       by-component))
    
    `((summary . ((total . ,total)
                  (successful . ,successful)
                  (failed . ,failed)
                  (success_rate . ,success-rate)))
      (by_component . ,by-component))))

;; Group deployments by component
(define (group-by-component deployments)
  "Group deployments by component type"
  (let ((groups '()))
    (for-each
     (lambda (deployment)
       (let* ((pr (assoc-ref deployment 'pr))
              (component (assoc-ref pr 'component))
              (existing (assoc-ref groups component)))
         (if existing
             (set! groups (assoc-set! groups component (cons deployment existing)))
             (set! groups (cons (cons component (list deployment)) groups)))))
     deployments)
    groups))

;; Demo function for 20 deployments
(define (demo-20-deployments)
  "Demo function to process 20 deployments with nice output"
  (format #t "🎬 ITIL Deployment Pipeline Demo~%")
  (format #t "==============================~%~%")
  
  (let ((all-prs (generate-mock-prs)))
    (format #t "🎯 Processing 20 deployments from 100 generated PRs...~%~%")
    (simulate-deployment-pipeline all-prs 20)))

;; Time utilities
(define (time->iso8601-string time)
  "Convert SRFI-19 time to ISO8601 string"
  (date->string (time-utc->date time) "~Y-~m-~dT~H:~M:~S"))

;; Module initialization
(format #t "~%=== Deployment Pipeline Simulator Loaded ===~%")
(format #t "Ready to simulate realistic deployment workflows!~%")
(format #t "Run (demo-20-deployments) to start the demo~%~%")