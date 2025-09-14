(define-module (main)
  #:use-module (risk calculator)
  #:use-module (risk factors)
  #:use-module (risk categories)
  #:use-module (risk freeze)
  #:re-export (;; Main risk calculation functions from calculator
               calculate-risk
               calculate-change-risk
               assess-risk
               calculate-title-risk
               calculate-system-risk
               calculate-time-risk

               ;; Risk categorization from categories
               categorize-risk
               get-risk-color
               get-approval-requirement
               risk-category-info

               ;; Freeze period management from freeze
               in-freeze-period?
               get-next-window
               check-blackout-dates
               get-freeze-risk-modifier

               ;; Risk factors from factors
               risk-factors
               high-risk-keywords
               get-factor-weight)
  #:export (;; Convenience functions
            risk-engine-version
            risk-engine-info
            assess-change-request))

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

(define (assess-change-request request-json)
  "Main entry point for risk assessment from MCP server
   Expects: ((title . \"string\") (description . \"string\") (systems . (list)))
   Returns: Complete risk assessment with score, category, and recommendations"
  (let* ((title (or (assoc-ref request-json 'title) ""))
         (description (or (assoc-ref request-json 'description) ""))
         (systems (or (assoc-ref request-json 'systems) '()))
         (assessment (assess-risk title description systems)))
    assessment))