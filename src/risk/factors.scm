(define-module (risk factors)
  #:export (risk-factors
            get-factor-weight
            high-risk-keywords
            calculate-keyword-risk))

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
    "purge" "wipe" "reset" "critical" "emergency"))

(define (get-factor-weight factor)
  "Get the weight for a specific risk factor"
  (or (assoc-ref risk-factors factor) 10))

(define (calculate-keyword-risk text keywords)
  "Calculate risk based on presence of keywords in text"
  (let ((lower-text (string-downcase text)))
    (fold (lambda (keyword total)
            (if (string-contains lower-text keyword)
                (+ total (get-factor-weight (string->symbol keyword)))
                total))
          0
          keywords)))