;;; Release Environment Pattern Simulator
;;; Demonstrates various deployment topologies and branching strategies

(define-module (simulator release-environment-simulator)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-43)
  #:export (simulate-release-pattern
            simulate-git-flow
            simulate-deployment-pipeline
            compare-patterns
            calculate-deployment-metrics))

;;; Data Types

(define-record-type <environment>
  (make-environment name type health version deploying? traffic-pct)
  environment?
  (name environment-name)
  (type environment-type)
  (health environment-health set-environment-health!)
  (version environment-version set-environment-version!)
  (deploying? environment-deploying? set-environment-deploying!)
  (traffic-pct environment-traffic-pct set-environment-traffic-pct!))

(define-record-type <deployment>
  (make-deployment id change-type source target version risk-score approval-status start-time duration)
  deployment?
  (id deployment-id)
  (change-type deployment-change-type)
  (source deployment-source)
  (target deployment-target)
  (version deployment-version)
  (risk-score deployment-risk-score)
  (approval-status deployment-approval-status set-deployment-approval-status!)
  (start-time deployment-start-time)
  (duration deployment-duration set-deployment-duration!))

(define-record-type <release-pattern>
  (make-release-pattern name topology environments deployments metrics)
  release-pattern?
  (name pattern-name)
  (topology pattern-topology)
  (environments pattern-environments)
  (deployments pattern-deployments set-pattern-deployments!)
  (metrics pattern-metrics set-pattern-metrics!))

;;; Environment Patterns

(define (create-linear-pipeline)
  "Create traditional dev -> qa -> staging -> production pipeline"
  (list
   (make-environment "dev" 'development "healthy" "1.4.5-dev" #f 0)
   (make-environment "qa" 'testing "healthy" "1.4.4" #f 0)
   (make-environment "staging" 'staging "healthy" "1.4.4" #f 0)
   (make-environment "production" 'production "healthy" "1.4.3" #f 100)))

(define (create-parallel-validation)
  "Create parallel staging and UAT environments"
  (list
   (make-environment "build" 'ci "healthy" "1.4.5-SNAPSHOT" #f 0)
   (make-environment "staging" 'staging "healthy" "1.4.4" #f 0)
   (make-environment "uat" 'uat "healthy" "1.4.4" #f 0)
   (make-environment "production" 'production "healthy" "1.4.3" #f 100)))

(define (create-multi-tenant-staging n-sandboxes n-stagings)
  "Create multiple sandbox and staging environments"
  (append
   ;; Sandboxes
   (map (lambda (i)
          (make-environment
           (format #f "sandbox-~a" i)
           'sandbox
           "healthy"
           (format #f "1.4.5-feature-~a" i)
           #f
           0))
        (iota n-sandboxes))
   ;; Stagings
   (map (lambda (i)
          (make-environment
           (format #f "staging-~a" i)
           'staging
           "healthy"
           (format #f "1.4.4-rc~a" i)
           #f
           0))
        (iota n-stagings))
   ;; Production
   (list (make-environment "production" 'production "healthy" "1.4.3" #f 100))))

(define (create-ring-deployment)
  "Create ring deployment pattern"
  (list
   (make-environment "staging" 'staging "healthy" "1.4.5-rc1" #f 0)
   (make-environment "ring-0" 'production "healthy" "1.4.4" #f 1)    ; Internal users
   (make-environment "ring-1" 'production "healthy" "1.4.4" #f 5)    ; Early adopters
   (make-environment "ring-2" 'production "healthy" "1.4.3" #f 25)   ; General availability
   (make-environment "ring-3" 'production "healthy" "1.4.3" #f 69))) ; Full rollout

(define (create-blue-green)
  "Create blue-green deployment pattern"
  (list
   (make-environment "staging" 'staging "healthy" "1.4.5-rc1" #f 0)
   (make-environment "blue" 'production "healthy" "1.4.4" #t 100)  ; Active
   (make-environment "green" 'production "healthy" "1.4.3" #f 0))) ; Standby

(define (create-canary-deployment)
  "Create canary deployment pattern"
  (list
   (make-environment "staging" 'staging "healthy" "1.4.5-rc1" #f 0)
   (make-environment "canary" 'production "healthy" "1.4.4" #f 5)    ; 5% traffic
   (make-environment "stable" 'production "healthy" "1.4.3" #f 95))) ; 95% traffic

(define (create-gitops-clusters)
  "Create GitOps multi-cluster pattern"
  (list
   (make-environment "dev-cluster" 'development "healthy" "develop-HEAD" #f 0)
   (make-environment "staging-cluster" 'staging "healthy" "staging-HEAD" #f 0)
   (make-environment "prod-cluster" 'production "healthy" "main-HEAD" #f 100)))

;;; Deployment Simulation

(define (simulate-deployment source-env target-env change-type)
  "Simulate a deployment from source to target environment"
  (let* ([version (environment-version source-env)]
         [risk-score (calculate-risk-score change-type target-env)]
         [approval (determine-approval-requirement change-type risk-score)]
         [deployment-id (format #f "CHG-~a" (random 10000))])

    (make-deployment
     deployment-id
     change-type
     (environment-name source-env)
     (environment-name target-env)
     version
     risk-score
     approval
     (current-time)
     0)))

(define (calculate-risk-score change-type target-env)
  "Calculate risk score based on change type and target environment"
  (let ([base-risk (case change-type
                     ((standard) 10)
                     ((normal) 25)
                     ((emergency) 40)
                     (else 15))]
        [env-multiplier (case (environment-type target-env)
                          ((production) 3.0)
                          ((staging uat) 1.5)
                          ((qa testing) 1.0)
                          ((development sandbox) 0.5)
                          (else 1.0))])
    (inexact->exact (round (* base-risk env-multiplier)))))

(define (determine-approval-requirement change-type risk-score)
  "Determine approval requirement based on change type and risk"
  (cond
   [(eq? change-type 'emergency) 'emergency-cab]
   [(> risk-score 50) 'executive-approval]
   [(> risk-score 30) 'cab-required]
   [(> risk-score 15) 'team-lead-approval]
   [else 'auto-approved]))

;;; Git Flow Simulation

(define (simulate-git-flow)
  "Simulate Git Flow branching strategy"
  (let ([branches '()]
        [environments (create-linear-pipeline)])

    ;; Create feature branch
    (set! branches (cons '(feature/add-oauth . develop) branches))
    (format #t "~%=== Git Flow Simulation ===~%")
    (format #t "1. Created feature/add-oauth from develop~%")

    ;; Deploy to dev
    (format #t "2. Feature branch deployed to DEV environment~%")
    (set-environment-version! (car environments) "1.4.5-feature-oauth")

    ;; Merge to develop
    (set! branches (cons '(develop . "1.4.5-dev") branches))
    (format #t "3. Feature merged to develop~%")

    ;; Create release branch
    (set! branches (cons '(release/1.4.5 . develop) branches))
    (format #t "4. Created release/1.4.5 from develop~%")
    (format #t "   - Deployed to QA for testing~%")
    (set-environment-version! (cadr environments) "1.4.5-rc1")

    ;; Release to staging
    (format #t "5. Release candidate deployed to STAGING~%")
    (set-environment-version! (caddr environments) "1.4.5-rc1")

    ;; Merge to main and deploy to production
    (format #t "6. Release merged to main~%")
    (format #t "   - CAB Approval required~%")
    (format #t "   - Deployed to PRODUCTION~%")
    (set-environment-version! (cadddr environments) "1.4.5")

    ;; Simulate hotfix
    (format #t "~%7. HOTFIX: Critical bug in production!~%")
    (set! branches (cons '(hotfix/1.4.6 . main) branches))
    (format #t "   - Created hotfix/1.4.6 from main~%")
    (format #t "   - Emergency CAB approval~%")
    (format #t "   - Direct deployment to PRODUCTION~%")
    (set-environment-version! (cadddr environments) "1.4.6")

    environments))

(define (simulate-github-flow)
  "Simulate GitHub Flow branching strategy"
  (format #t "~%=== GitHub Flow Simulation ===~%")
  (let ([main-version "1.4.4"]
        [pr-count 0])

    ;; Feature branch 1
    (set! pr-count (+ pr-count 1))
    (format #t "1. Created feature/improve-performance~%")
    (format #t "   - Opened PR #~a~%" pr-count)
    (format #t "   - Review app deployed~%")
    (format #t "   - Tests passing~%")
    (format #t "   - Merged to main~%")

    ;; Auto-deploy to staging
    (format #t "2. Auto-deployed to STAGING~%")
    (set! main-version "1.4.5")

    ;; Feature branch 2
    (set! pr-count (+ pr-count 1))
    (format #t "3. Created feature/add-monitoring~%")
    (format #t "   - Opened PR #~a~%" pr-count)
    (format #t "   - Review app deployed~%")
    (format #t "   - Tests passing~%")
    (format #t "   - Merged to main~%")

    ;; Tag for production
    (format #t "4. Tagged v~a for production release~%" main-version)
    (format #t "   - Automated deployment to PRODUCTION~%")

    main-version))

;;; Pipeline Execution

(define (execute-linear-pipeline environments)
  "Execute deployment through linear pipeline"
  (format #t "~%=== Executing Linear Pipeline ===~%")
  (let ([deployments '()])
    ;; DEV -> QA
    (format #t "Stage 1: DEV -> QA~%")
    (let ([dep (simulate-deployment (car environments) (cadr environments) 'standard)])
      (format #t "  Deployment ~a: ~a~%" (deployment-id dep) (deployment-approval-status dep))
      (set! deployments (cons dep deployments)))

    ;; QA -> Staging
    (format #t "Stage 2: QA -> STAGING~%")
    (let ([dep (simulate-deployment (cadr environments) (caddr environments) 'normal)])
      (format #t "  Deployment ~a: ~a~%" (deployment-id dep) (deployment-approval-status dep))
      (set! deployments (cons dep deployments)))

    ;; Staging -> Production
    (format #t "Stage 3: STAGING -> PRODUCTION~%")
    (let ([dep (simulate-deployment (caddr environments) (cadddr environments) 'normal)])
      (format #t "  Deployment ~a: ~a~%" (deployment-id dep) (deployment-approval-status dep))
      (format #t "  Risk Score: ~a~%" (deployment-risk-score dep))
      (set! deployments (cons dep deployments)))

    (reverse deployments)))

(define (execute-parallel-validation environments)
  "Execute parallel validation deployment"
  (format #t "~%=== Executing Parallel Validation ===~%")

  ;; Deploy to staging and UAT in parallel
  (format #t "Parallel Deployment:~%")
  (format #t "  ├── Deploying to STAGING...~%")
  (format #t "  └── Deploying to UAT...~%")

  (thread-sleep! 1) ; Simulate parallel execution

  (format #t "~%Validation Results:~%")
  (format #t "  STAGING: ✅ All tests passed~%")
  (format #t "  UAT: ✅ Business approval received~%")

  (format #t "~%Promoting to PRODUCTION:~%")
  (format #t "  CAB Review: APPROVED~%")
  (format #t "  Deployment: SUCCESS~%"))

(define (execute-ring-deployment environments)
  "Execute ring deployment strategy"
  (format #t "~%=== Executing Ring Deployment ===~%")

  (let ([rings (cdr environments)]) ; Skip staging
    (format #t "Progressive Rollout:~%")

    ;; Ring 0 - Internal users (1%)
    (format #t "~%Ring 0 (Internal): 1% traffic~%")
    (set-environment-version! (car rings) "1.4.5")
    (format #t "  Monitoring for 1 hour...~%")
    (format #t "  Metrics: ✅ No issues~%")

    ;; Ring 1 - Early adopters (5%)
    (format #t "~%Ring 1 (Early Adopters): 5% traffic~%")
    (set-environment-version! (cadr rings) "1.4.5")
    (format #t "  Monitoring for 4 hours...~%")
    (format #t "  Metrics: ✅ Error rate: 0.02%~%")

    ;; Ring 2 - General availability (25%)
    (format #t "~%Ring 2 (General): 25% traffic~%")
    (set-environment-version! (caddr rings) "1.4.5")
    (format #t "  Monitoring for 24 hours...~%")
    (format #t "  Metrics: ✅ Performance stable~%")

    ;; Ring 3 - Full rollout (100%)
    (format #t "~%Ring 3 (Full Rollout): 100% traffic~%")
    (set-environment-version! (cadddr rings) "1.4.5")
    (format #t "  Deployment complete!~%")))

(define (execute-blue-green-switch environments)
  "Execute blue-green deployment switch"
  (format #t "~%=== Executing Blue-Green Switch ===~%")

  (let ([staging (car environments)]
        [blue (cadr environments)]
        [green (caddr environments)])

    (format #t "Current State:~%")
    (format #t "  BLUE (Active): v~a - 100% traffic~%" (environment-version blue))
    (format #t "  GREEN (Standby): v~a - 0% traffic~%" (environment-version green))

    (format #t "~%Deployment Process:~%")
    (format #t "1. Deploy v1.4.5 to GREEN environment~%")
    (set-environment-version! green "1.4.5")

    (format #t "2. Run smoke tests on GREEN~%")
    (format #t "   ✅ All tests passed~%")

    (format #t "3. Switch traffic: BLUE -> GREEN~%")
    (set-environment-traffic-pct! blue 0)
    (set-environment-traffic-pct! green 100)

    (format #t "~%New State:~%")
    (format #t "  BLUE (Standby): v~a - 0% traffic~%" (environment-version blue))
    (format #t "  GREEN (Active): v~a - 100% traffic~%" (environment-version green))

    (format #t "~%Rollback available: instant switch back to BLUE~%")))

(define (execute-canary-deployment environments)
  "Execute canary deployment"
  (format #t "~%=== Executing Canary Deployment ===~%")

  (let ([staging (car environments)]
        [canary (cadr environments)]
        [stable (caddr environments)])

    (format #t "Initial State:~%")
    (format #t "  Stable: v~a - 95% traffic~%" (environment-version stable))
    (format #t "  Canary: v~a - 5% traffic~%" (environment-version canary))

    (format #t "~%Progressive Canary Rollout:~%")

    ;; 5% canary
    (format #t "~%Phase 1: 5% canary traffic~%")
    (format #t "  Monitoring metrics...~%")
    (format #t "  Error rate: 0.01% ✅~%")
    (format #t "  Latency: +2ms ✅~%")

    ;; 25% canary
    (format #t "~%Phase 2: Expanding to 25%~%")
    (set-environment-traffic-pct! canary 25)
    (set-environment-traffic-pct! stable 75)
    (format #t "  Monitoring metrics...~%")
    (format #t "  Error rate: 0.02% ✅~%")
    (format #t "  Latency: +3ms ✅~%")

    ;; 50% canary
    (format #t "~%Phase 3: Expanding to 50%~%")
    (set-environment-traffic-pct! canary 50)
    (set-environment-traffic-pct! stable 50)
    (format #t "  Monitoring metrics...~%")
    (format #t "  Error rate: 0.03% ✅~%")
    (format #t "  Latency: +1ms ✅~%")

    ;; Full rollout
    (format #t "~%Phase 4: Full rollout~%")
    (set-environment-traffic-pct! canary 100)
    (set-environment-traffic-pct! stable 0)
    (set-environment-version! stable (environment-version canary))
    (format #t "  Canary promotion complete!~%")
    (format #t "  All traffic on v~a~%" (environment-version stable))))

;;; Pattern Comparison

(define (calculate-deployment-metrics pattern deployments)
  "Calculate deployment metrics for a pattern"
  (let ([total-deployments (length deployments)]
        [failed-deployments 0]
        [total-duration 0]
        [rollback-count 0])

    ;; Simulate some metrics
    (set! failed-deployments (inexact->exact (floor (* total-deployments 0.05))))
    (set! total-duration (* total-deployments 45)) ; 45 min average
    (set! rollback-count (inexact->exact (floor (* failed-deployments 0.5))))

    `((deployment-frequency . ,(/ total-deployments 30.0)) ; per day
      (lead-time . ,(/ total-duration total-deployments))  ; minutes
      (mttr . 22)                                           ; minutes
      (change-failure-rate . ,(/ failed-deployments (max 1 total-deployments)))
      (rollback-rate . ,(/ rollback-count (max 1 total-deployments))))))

(define (compare-patterns)
  "Compare different deployment patterns"
  (format #t "~%=== Deployment Pattern Comparison ===~%")
  (format #t "~%┌─────────────────┬──────────┬───────────┬──────┬─────────────┐~%")
  (format #t "│ Pattern         │ Frequency│ Lead Time │ MTTR │ Failure Rate│~%")
  (format #t "├─────────────────┼──────────┼───────────┼──────┼─────────────┤~%")

  (let ([patterns
         '(("Linear Pipeline" 0.2 "5 days" "4 hrs" "5%")
           ("Parallel Valid" 1.0 "2 days" "2 hrs" "3%")
           ("Multi-Tenant" 24.0 "4 hrs" "30 min" "2%")
           ("Ring Deploy" 1.0 "1 day" "15 min" "1%")
           ("Blue-Green" 1.0 "4 hrs" "5 min" "0.5%")
           ("Canary" 4.0 "2 hrs" "10 min" "0.5%")
           ("GitOps" 48.0 "30 min" "5 min" "0.1%"))])

    (for-each
     (lambda (pattern)
       (format #t "│ ~15a │ ~8a │ ~9a │ ~4a │ ~11a │~%"
               (car pattern)
               (format #f "~a/day" (cadr pattern))
               (caddr pattern)
               (cadddr pattern)
               (car (cddddr pattern))))
     patterns))

  (format #t "└─────────────────┴──────────┴───────────┴──────┴─────────────┘~%"))

;;; Main Simulation Function

(define (simulate-release-pattern pattern-name)
  "Simulate a specific release pattern"
  (format #t "~%╔══════════════════════════════════════════════════════╗~%")
  (format #t "║     Release Pattern Simulation: ~20a║~%" pattern-name)
  (format #t "╚══════════════════════════════════════════════════════╝~%")

  (case pattern-name
    [(linear)
     (let ([envs (create-linear-pipeline)])
       (execute-linear-pipeline envs))]

    [(parallel)
     (let ([envs (create-parallel-validation)])
       (execute-parallel-validation envs))]

    [(multi-tenant)
     (let ([envs (create-multi-tenant-staging 3 3)])
       (format #t "~%Multi-Tenant Environment:~%")
       (for-each (lambda (env)
                   (format #t "  - ~a: v~a~%"
                           (environment-name env)
                           (environment-version env)))
                 envs))]

    [(ring)
     (let ([envs (create-ring-deployment)])
       (execute-ring-deployment envs))]

    [(blue-green)
     (let ([envs (create-blue-green)])
       (execute-blue-green-switch envs))]

    [(canary)
     (let ([envs (create-canary-deployment)])
       (execute-canary-deployment envs))]

    [(git-flow)
     (simulate-git-flow)]

    [(github-flow)
     (simulate-github-flow)]

    [(comparison)
     (compare-patterns)]

    [else
     (format #t "Unknown pattern: ~a~%" pattern-name)
     (format #t "Available patterns: linear, parallel, multi-tenant, ring,~%")
     (format #t "                   blue-green, canary, git-flow, github-flow, comparison~%")]))

;;; Demo Runner

(define (run-all-simulations)
  "Run all deployment pattern simulations"
  (for-each simulate-release-pattern
            '(linear parallel ring blue-green canary git-flow github-flow comparison)))

;; Export for external use
(define (simulate-deployment-pipeline)
  "Main entry point for deployment pipeline simulation"
  (format #t "~%════════════════════════════════════════════════════════~%")
  (format #t "     RELEASE ENVIRONMENT PATTERN SIMULATOR~%")
  (format #t "════════════════════════════════════════════════════════~%")

  (format #t "~%Select a simulation:~%")
  (format #t "1. Linear Pipeline (dev->qa->stg->prd)~%")
  (format #t "2. Parallel Validation ([stg,uat]->prd)~%")
  (format #t "3. Ring Deployment (progressive rollout)~%")
  (format #t "4. Blue-Green Switch~%")
  (format #t "5. Canary Deployment~%")
  (format #t "6. Git Flow Demo~%")
  (format #t "7. GitHub Flow Demo~%")
  (format #t "8. Pattern Comparison~%")
  (format #t "9. Run All Simulations~%")

  ;; For non-interactive mode, run comparison
  (simulate-release-pattern 'comparison))