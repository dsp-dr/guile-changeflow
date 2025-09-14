(define-module (risk categories)
  #:export (categorize-risk
            get-risk-color
            get-approval-requirement
            risk-category-info))

(define (categorize-risk score)
  "Categorize risk score into low/medium/high/critical"
  (cond
    ((< score 31) 'low)      ; 0-30 = low
    ((< score 71) 'medium)   ; 31-70 = medium
    ((< score 91) 'high)     ; 71-90 = high
    (else 'critical)))       ; 91-100 = critical

(define (get-risk-color category)
  "Get color for risk category (for web UI)"
  (case category
    ((low) "green")
    ((medium) "yellow")
    ((high) "orange")
    ((critical) "red")
    (else "gray")))

(define (get-approval-requirement category)
  "How many approvals needed based on risk"
  (case category
    ((low) 1)
    ((medium) 2)
    ((high) 3)
    ((critical) 5)
    (else 1)))

(define (risk-category-info score)
  "Get complete risk category information for a score"
  (let ((category (categorize-risk score)))
    `((score . ,score)
      (category . ,category)
      (color . ,(get-risk-color category))
      (approvals-required . ,(get-approval-requirement category))
      (description . ,(case category
                       ((low) "Low risk - standard approval process")
                       ((medium) "Medium risk - requires additional review")
                       ((high) "High risk - requires senior approval")
                       ((critical) "Critical risk - requires emergency review")
                       (else "Unknown risk level"))))))