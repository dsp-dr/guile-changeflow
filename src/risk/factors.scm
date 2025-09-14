(define-module (risk factors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (risk-factors
            get-factor-weight
            high-risk-keywords
            critical-systems
            get-system-risk-weight))

(define risk-factors
  '((production . 40)
    (security . 35)
    (payment . 45)
    (database . 30)
    (authentication . 35)
    (infrastructure . 25)
    (configuration . 15)
    (documentation . 5)
    (testing . 10)
    (api . 20)
    (migration . 30)
    (backup . 15)
    (critical . 50)
    (emergency . 45)
    (hotfix . 35)))

(define high-risk-keywords
  '("delete" "drop" "truncate" "production" "payment" "security"
    "auth" "credential" "password" "secret" "key" "token"
    "purge" "wipe" "reset" "critical" "emergency" "rollback"
    "firewall" "encryption" "compliance" "gdpr" "pci" "audit"))

(define (get-factor-weight factor)
  "Get the weight for a specific risk factor"
  (or (assoc-ref risk-factors factor) 10))

(define critical-systems
  '("payment-gateway" "auth-service" "database-master" "api-gateway"
    "loadbalancer" "firewall" "security-scanner" "audit-log"
    "user-management" "billing" "subscription" "compliance"
    "encryption-service" "vault" "secrets-manager" "monitoring"
    "alerting" "logging" "kafka" "redis" "elasticsearch"))

(define (get-system-risk-weight system-name)
  "Get risk weight for a specific system"
  (cond
    ((member system-name critical-systems) 30)
    ((string-contains system-name "prod") 25)
    ((string-contains system-name "payment") 35)
    ((string-contains system-name "auth") 30)
    ((string-contains system-name "database") 25)
    ((string-contains system-name "api") 20)
    ((string-contains system-name "cache") 15)
    ((string-contains system-name "queue") 15)
    (else 10)))

