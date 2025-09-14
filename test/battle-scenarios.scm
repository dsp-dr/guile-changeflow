#!/usr/bin/env guile
!#

;;; Battle Scenario Test Suite - 15 Years of Enterprise Scenarios
;;; Tests the risk engine against real-world scenarios from 2010-2025

(add-to-load-path "src")
(use-modules (risk calculator)
             (risk categories)
             (ice-9 format))

(define battle-scenarios
  '(;; Year 2010 - Early cloud migration era
    ((year . 2010)
     (title . "Migrate on-premise database to AWS RDS")
     (description . "Move production Oracle database to cloud")
     (systems . ("database-master" "api-gateway" "auth-service"))
     (expected-category . high))

    ;; Year 2011 - Mobile app boom
    ((year . 2011)
     (title . "Deploy new mobile API endpoints")
     (description . "Add REST APIs for iOS and Android apps")
     (systems . ("api-gateway"))
     (expected-category . medium))

    ;; Year 2012 - Security breach responses
    ((year . 2012)
     (title . "URGENT: Patch SQL injection vulnerability")
     (description . "Critical security fix for payment system")
     (systems . ("payment-gateway" "database-master"))
     (expected-category . critical))

    ;; Year 2013 - Big data adoption
    ((year . 2013)
     (title . "Deploy Hadoop cluster for analytics")
     (description . "New big data infrastructure")
     (systems . ("analytics" "database-master"))
     (expected-category . high))

    ;; Year 2014 - Microservices transition
    ((year . 2014)
     (title . "Break monolith into microservices")
     (description . "Decompose main application")
     (systems . ("api-gateway" "auth-service" "payment-gateway" "user-management"))
     (expected-category . critical))

    ;; Year 2015 - Container adoption
    ((year . 2015)
     (title . "Migrate services to Docker containers")
     (description . "Containerize all microservices")
     (systems . ("api-gateway" "loadbalancer"))
     (expected-category . high))

    ;; Year 2016 - GDPR preparation
    ((year . 2016)
     (title . "Implement GDPR compliance changes")
     (description . "Update user data handling for privacy")
     (systems . ("user-management" "database-master" "audit-log"))
     (expected-category . high))

    ;; Year 2017 - Kubernetes migration
    ((year . 2017)
     (title . "Move production to Kubernetes")
     (description . "Orchestration platform migration")
     (systems . ("loadbalancer" "api-gateway"))
     (expected-category . high))

    ;; Year 2018 - GDPR deadline
    ((year . 2018)
     (title . "EMERGENCY: GDPR compliance hotfix")
     (description . "Critical privacy regulation compliance")
     (systems . ("user-management" "audit-log" "database-master"))
     (expected-category . critical))

    ;; Year 2019 - Multi-cloud strategy
    ((year . 2019)
     (title . "Deploy backup systems to Azure")
     (description . "Multi-cloud disaster recovery")
     (systems . ("backup" "database-master"))
     (expected-category . medium))

    ;; Year 2020 - Pandemic response
    ((year . 2020)
     (title . "URGENT: Scale for 10x traffic increase")
     (description . "Emergency scaling for remote work surge")
     (systems . ("loadbalancer" "api-gateway" "database-master" "redis" "kafka"))
     (expected-category . critical))

    ;; Year 2021 - Zero trust security
    ((year . 2021)
     (title . "Implement zero trust network architecture")
     (description . "Security overhaul for authentication")
     (systems . ("auth-service" "firewall" "vault"))
     (expected-category . high))

    ;; Year 2022 - Supply chain security
    ((year . 2022)
     (title . "Emergency patch for Log4j vulnerability")
     (description . "Critical security update across all Java services")
     (systems . ("api-gateway" "payment-gateway" "auth-service" "kafka" "elasticsearch"))
     (expected-category . critical))

    ;; Year 2023 - AI integration
    ((year . 2023)
     (title . "Deploy ChatGPT integration to production")
     (description . "Add AI capabilities to customer service")
     (systems . ("api-gateway" "redis"))
     (expected-category . medium))

    ;; Year 2024 - Quantum-ready encryption
    ((year . 2024)
     (title . "Upgrade to quantum-resistant encryption")
     (description . "Future-proof security infrastructure")
     (systems . ("encryption-service" "vault" "auth-service"))
     (expected-category . high))

    ;; Year 2025 - Current year scenario
    ((year . 2025)
     (title . "Production payment system database migration")
     (description . "Critical payment infrastructure upgrade during Black Friday")
     (systems . ("payment-gateway" "database-master" "auth-service" "billing"))
     (expected-category . critical))))

(define (run-battle-scenarios)
  "Run all battle scenarios and check results"
  (display "==============================================\n")
  (display "   BATTLE SCENARIO TEST SUITE (2010-2025)    \n")
  (display "==============================================\n\n")

  (let ((passed 0)
        (failed 0)
        (total (length battle-scenarios)))

    (for-each
     (lambda (scenario)
       (let* ((year (assoc-ref scenario 'year))
              (title (assoc-ref scenario 'title))
              (description (assoc-ref scenario 'description))
              (systems (assoc-ref scenario 'systems))
              (expected (assoc-ref scenario 'expected-category))
              (result (assess-risk title description systems))
              (score (assoc-ref result 'risk-score))
              (category (assoc-ref result 'risk-category))
              (pass? (eq? category expected)))

         (display (format #f "~a [~a]: ~a\n"
                         year
                         (if pass? "PASS" "FAIL")
                         title))
         (display (format #f "  Score: ~a/100, Category: ~a (expected: ~a)\n"
                         score category expected))

         (if pass?
             (set! passed (+ passed 1))
             (begin
               (set! failed (+ failed 1))
               (display (format #f "  >>> MISMATCH: Got ~a, expected ~a\n"
                               category expected))))
         (newline)))
     battle-scenarios)

    ;; Summary
    (display "==============================================\n")
    (display (format #f "RESULTS: ~a/~a passed, ~a failed\n" passed total failed))
    (display (format #f "Success rate: ~a%\n"
                    (inexact->exact (round (* 100 (/ passed total))))))

    ;; Risk distribution analysis
    (display "\nRisk Distribution:\n")
    (let ((categories '()))
      (for-each
       (lambda (scenario)
         (let* ((result (assess-risk (assoc-ref scenario 'title)
                                     (assoc-ref scenario 'description)
                                     (assoc-ref scenario 'systems)))
                (category (assoc-ref result 'risk-category)))
           (set! categories (cons category categories))))
       battle-scenarios)

      (for-each
       (lambda (cat)
         (let ((count (length (filter (lambda (c) (eq? c cat)) categories))))
           (display (format #f "  ~a: ~a scenarios\n" cat count))))
       '(low medium high critical)))

    (display "\n==============================================\n")
    (display "Battle scenarios complete. System battle-tested!\n")))

;; Run the battle scenarios
(run-battle-scenarios)