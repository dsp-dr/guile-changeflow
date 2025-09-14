(define-module (risk calculator)
  #:use-module (risk factors)
  #:use-module (risk freeze)
  #:use-module (risk categories)
  #:use-module (srfi srfi-13)  ; string operations
  #:use-module (srfi srfi-1)   ; list operations
  #:use-module (srfi srfi-19)  ; time operations
  #:export (calculate-risk
            calculate-change-risk
            calculate-title-risk
            calculate-system-risk
            calculate-time-risk
            calculate-keyword-risk
            assess-risk))

(define (calculate-risk title description systems)
  "Calculate risk score from 0-100 based on various factors"
  (let ((base-score 10)
        (title-risk (calculate-title-risk title))
        (desc-risk (if (string? description)
                       (calculate-keyword-risk description high-risk-keywords)
                       0))
        (system-risk (calculate-system-risk systems))
        (time-risk (calculate-time-risk)))

    (min 100 (+ base-score title-risk desc-risk system-risk time-risk))))

(define (calculate-change-risk change-request)
  "Calculate risk for a complete change request object"
  (if (not (list? change-request))
      50  ; Default medium risk if invalid input
      (let ((title (or (assoc-ref change-request 'title) ""))
            (description (or (assoc-ref change-request 'description) ""))
            (systems (or (assoc-ref change-request 'systems) '())))
        (calculate-risk title description systems))))

(define (calculate-title-risk title)
  "Check for risky keywords in title"
  (if (not (string? title))
      0
      (calculate-keyword-risk title high-risk-keywords)))

(define (calculate-keyword-risk text keywords)
  "Calculate risk based on presence of keywords in text"
  (if (not (string? text))
      0
      (fold (lambda (keyword score)
              (if (string-contains-ci text keyword)
                  (+ score (get-factor-weight (string->symbol keyword)))
                  score))
            0
            keywords)))

(define (calculate-system-risk systems)
  "More systems = higher risk"
  (cond
    ((not (list? systems)) 5)  ; Invalid input gets small penalty
    ((null? systems) 0)
    ((= (length systems) 1) 10)
    ((= (length systems) 2) 25)
    ((= (length systems) 3) 35)
    (else 40)))

(define (calculate-time-risk)
  "Weekend or after hours = higher risk"
  (get-freeze-risk-modifier))

(define (assess-risk title description systems)
  "Complete risk assessment with categorization"
  (let* ((score (calculate-risk title description systems))
         (category (categorize-risk score))
         (freeze-status (in-freeze-period?))
         (next-window (get-next-window)))

    `((risk-score . ,score)
      (risk-category . ,category)
      (risk-color . ,(get-risk-color category))
      (approvals-required . ,(get-approval-requirement category))
      (in-freeze-period . ,freeze-status)
      (next-deployment-window . ,next-window)
      (recommendation . ,(if freeze-status
                             "Deployment not recommended - freeze period active"
                             (case category
                               ((low) "Approved for deployment")
                               ((medium) "Requires review before deployment")
                               ((high) "Requires senior approval")
                               ((critical) "Emergency review required")
                               (else "Unknown risk level")))))))

(define (risk-summary score)
  "Generate human readable risk summary"
  (let ((category (categorize-risk score)))
    (format #f "Risk Score: ~a/100 (~a)" score category)))