(define-module (main)
  #:use-module (risk calculator)
  #:use-module (risk factors)
  #:use-module (risk categories)
  #:use-module (risk freeze)
  #:export (;; Main risk calculation functions
            calculate-risk
            calculate-change-risk
            assess-risk

            ;; Component calculators
            calculate-title-risk
            calculate-system-risk
            calculate-time-risk

            ;; Risk categorization
            categorize-risk
            get-risk-color
            get-approval-requirement
            risk-category-info

            ;; Freeze period management
            in-freeze-period?
            get-next-window
            check-blackout-dates
            get-freeze-risk-modifier

            ;; Risk factors
            risk-factors
            high-risk-keywords
            get-factor-weight
            calculate-keyword-risk

            ;; Convenience functions
            risk-engine-version
            risk-engine-info))

(define risk-engine-version "1.0.0")

(define (risk-engine-info)
  "Get information about the risk engine"
  `((name . "GCF Risk Engine")
    (version . ,risk-engine-version)
    (description . "Risk assessment system for Guile ChangeFlow")
    (capabilities . ("risk-scoring" "freeze-period-detection" "approval-requirements"))
    (score-range . (0 . 100))
    (categories . ("low" "medium" "high" "critical"))
    (freeze-detection . #t)))