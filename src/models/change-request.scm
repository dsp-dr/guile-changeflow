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
            set-change-request-risk-score!
            set-change-request-status!
            set-change-request-updated-at!
            current-time))

(define-record-type <change-request>
  (make-change-request id title description risk-score status created-at updated-at)
  change-request?
  (id change-request-id)
  (title change-request-title)
  (description change-request-description)
  (risk-score change-request-risk-score set-change-request-risk-score!)
  (status change-request-status set-change-request-status!)
  (created-at change-request-created-at)
  (updated-at change-request-updated-at set-change-request-updated-at!))

(define (current-time)
  (date->string (current-date) "~Y-~m-~d ~H:~M:~S"))