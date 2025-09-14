(define-module (models approval)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (models change-request)
  #:use-module (models state-machine)
  #:export (make-approval
            approval?
            approval-id
            approval-change-id
            approval-approver
            approval-decision
            approval-comments
            approval-created-at
            create-approval
            approve-change!
            reject-change!
            request-more-info!
            can-approve-change?
            get-approval-status
            approval-decision-pending?
            approval-decision-approved?
            approval-decision-rejected?))

(define-record-type <approval>
  (%make-approval id change-id approver decision comments created-at)
  approval?
  (id approval-id)
  (change-id approval-change-id)
  (approver approval-approver)
  (decision approval-decision set-approval-decision!)
  (comments approval-comments set-approval-comments!)
  (created-at approval-created-at))

(define (make-approval id change-id approver decision comments created-at)
  (%make-approval id change-id approver decision comments created-at))

(define (create-approval change-id approver
                        #:key
                        (decision 'pending)
                        (comments ""))
  (let ((id (string-append "APP-" change-id "-" (symbol->string (gensym))))
        (now (current-time)))
    (%make-approval id change-id approver decision comments now)))

(define (approve-change! change approver comments)
  "Approve a change request if it's in the correct state"
  (let ((current-state (change-request-status change)))
    (if (can-approve? current-state)
        (begin
          (transition-change-state! change 'approved)
          (add-change-request-approver! change approver)
          (add-change-request-comment! change
                                       (format #f "Approved by ~a: ~a"
                                               approver comments))
          (create-approval (change-request-id change) approver
                          #:decision 'approved
                          #:comments comments))
        (error "Change cannot be approved in current state"
               (change-request-id change)
               current-state))))

(define (reject-change! change approver comments)
  "Reject a change request if it's in the correct state"
  (let ((current-state (change-request-status change)))
    (if (can-reject? current-state)
        (begin
          (transition-change-state! change 'rejected)
          (add-change-request-comment! change
                                       (format #f "Rejected by ~a: ~a"
                                               approver comments))
          (create-approval (change-request-id change) approver
                          #:decision 'rejected
                          #:comments comments))
        (error "Change cannot be rejected in current state"
               (change-request-id change)
               current-state))))

(define (request-more-info! change approver comments)
  "Request more information for a change request"
  (let ((current-state (change-request-status change)))
    (if (eq? current-state 'assessing)
        (begin
          (transition-change-state! change 'needs-info)
          (add-change-request-comment! change
                                       (format #f "More info requested by ~a: ~a"
                                               approver comments))
          (create-approval (change-request-id change) approver
                          #:decision 'needs-info
                          #:comments comments))
        (error "Cannot request more info for change in current state"
               (change-request-id change)
               current-state))))

(define (can-approve-change? change)
  "Check if a change can be approved in its current state"
  (can-approve? (change-request-status change)))

(define (get-approval-status change)
  "Get the overall approval status of a change"
  (case (change-request-status change)
    ((approved) 'approved)
    ((rejected) 'rejected)
    ((needs-info) 'needs-info)
    (else 'pending)))

(define (approval-decision-pending? approval)
  (eq? (approval-decision approval) 'pending))

(define (approval-decision-approved? approval)
  (eq? (approval-decision approval) 'approved))

(define (approval-decision-rejected? approval)
  (eq? (approval-decision approval) 'rejected))