(define-module (models change-request)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:export (make-change-request
            change-request?
            change-request-id
            change-request-title
            change-request-description
            change-request-risk-score
            change-request-status
            change-request-created-at
            change-request-updated-at
            change-request-submitter
            change-request-approvers
            change-request-comments
            change-request-attachments
            change-request-impact-level
            change-request-urgency
            change-request-category
            set-change-request-status!
            set-change-request-risk-score!
            set-change-request-updated-at!
            add-change-request-comment!
            add-change-request-approver!
            create-change-request))

(define-record-type <change-request>
  (%make-change-request id title description risk-score status created-at
                        updated-at submitter approvers comments attachments
                        impact-level urgency category)
  change-request?
  (id change-request-id)
  (title change-request-title)
  (description change-request-description)
  (risk-score change-request-risk-score set-change-request-risk-score!)
  (status change-request-status set-change-request-status!)
  (created-at change-request-created-at)
  (updated-at change-request-updated-at set-change-request-updated-at!)
  (submitter change-request-submitter)
  (approvers change-request-approvers set-change-request-approvers!)
  (comments change-request-comments set-change-request-comments!)
  (attachments change-request-attachments set-change-request-attachments!)
  (impact-level change-request-impact-level)
  (urgency change-request-urgency)
  (category change-request-category))

(define (make-change-request id title description risk-score status created-at updated-at)
  (%make-change-request id title description risk-score status created-at
                        updated-at #f '() '() '() 'medium 'normal 'standard))

(define (create-change-request id title description
                              #:key
                              (risk-score 0)
                              (status 'submitted)
                              (submitter #f)
                              (impact-level 'medium)
                              (urgency 'normal)
                              (category 'standard))
  (let ((now (current-time)))
    (%make-change-request id title description risk-score status
                          now now submitter '() '() '()
                          impact-level urgency category)))

(define (add-change-request-comment! change comment)
  (let ((comments (change-request-comments change)))
    (set-change-request-comments! change (cons comment comments))
    (set-change-request-updated-at! change (current-time))))

(define (add-change-request-approver! change approver)
  (let ((approvers (change-request-approvers change)))
    (unless (member approver approvers)
      (set-change-request-approvers! change (cons approver approvers))
      (set-change-request-updated-at! change (current-time)))))