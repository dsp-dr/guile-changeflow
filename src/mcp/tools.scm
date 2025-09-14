(define-module (mcp tools)
  #:use-module (models change-request)
  #:use-module (storage memory)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (mcp simulator-tools)
  #:export (create-change-request-tool
            assess-risk-tool
            simulator-tool-definitions
            handle-simulator-tool-call))

(define (generate-change-id)
  "Generate a unique change ID"
  (string-append "CHG-" (number->string (random 100000))))

(define (create-change-request-tool params)
  "Tool to create a new change request"
  (let* ((title (assoc-ref params 'title))
         (description (assoc-ref params 'description))
         (change-id (generate-change-id))
         (current-timestamp (current-time))
         (change (make-change-request
                  change-id
                  title
                  description
                  0  ; Initial risk score
                  'submitted
                  current-timestamp
                  current-timestamp)))

    ;; Store the change
    (store-change! change)

    ;; Return MCP response
    `((success . #t)
      (changeId . ,change-id)
      (title . ,title)
      (description . ,description)
      (status . "submitted")
      (createdAt . ,current-timestamp)
      (message . "Change request created successfully"))))

(define (assess-risk-tool params)
  "Tool to assess risk for a change request"
  (let ((change-id (assoc-ref params 'changeId)))
    (let ((change (get-change change-id)))
      (if change
          (let* ((risk-score (+ 10 (random 90)))
                 (risk-category (cond
                                 ((< risk-score 30) "low")
                                 ((< risk-score 70) "medium")
                                 (else "high"))))
            ;; Update the change with the risk score
            (set-change-request-risk-score! change risk-score)
            (set-change-request-updated-at! change (current-time))

            ;; Return MCP response
            `((success . #t)
              (changeId . ,change-id)
              (riskScore . ,risk-score)
              (category . ,risk-category)
              (assessedAt . ,(current-time))
              (message . "Risk assessment completed")))

          ;; Change not found
          `((success . #f)
            (error . "Change request not found")
            (changeId . ,change-id))))))