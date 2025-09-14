(define-module (models audit)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:export (make-audit-entry
            audit-entry?
            audit-entry-id
            audit-entry-change-id
            audit-entry-event-type
            audit-entry-old-value
            audit-entry-new-value
            audit-entry-user
            audit-entry-timestamp
            audit-entry-details
            create-audit-entry
            audit-state-change
            audit-field-change
            audit-comment-added
            audit-approver-added
            get-audit-trail))

(define-record-type <audit-entry>
  (%make-audit-entry id change-id event-type old-value new-value user timestamp details)
  audit-entry?
  (id audit-entry-id)
  (change-id audit-entry-change-id)
  (event-type audit-entry-event-type)
  (old-value audit-entry-old-value)
  (new-value audit-entry-new-value)
  (user audit-entry-user)
  (timestamp audit-entry-timestamp)
  (details audit-entry-details))

(define audit-trail '())

(define (make-audit-entry id change-id event-type old-value new-value user timestamp details)
  (%make-audit-entry id change-id event-type old-value new-value user timestamp details))

(define (create-audit-entry change-id event-type
                           #:key
                           (old-value #f)
                           (new-value #f)
                           (user "system")
                           (details ""))
  (let ((id (string-append "AUDIT-" (number->string (random 1000000))))
        (now (current-time)))
    (let ((entry (%make-audit-entry id change-id event-type old-value new-value
                                    user now details)))
      (set! audit-trail (cons entry audit-trail))
      entry)))

(define (audit-state-change change-id old-state new-state user)
  "Create audit entry for state change"
  (create-audit-entry change-id 'state-change
                     #:old-value old-state
                     #:new-value new-state
                     #:user user
                     #:details (format #f "State changed from ~a to ~a"
                                       old-state new-state)))

(define (audit-field-change change-id field-name old-value new-value user)
  "Create audit entry for field change"
  (create-audit-entry change-id 'field-change
                     #:old-value old-value
                     #:new-value new-value
                     #:user user
                     #:details (format #f "Field ~a changed from ~a to ~a"
                                       field-name old-value new-value)))

(define (audit-comment-added change-id comment user)
  "Create audit entry for comment addition"
  (create-audit-entry change-id 'comment-added
                     #:new-value comment
                     #:user user
                     #:details (format #f "Comment added by ~a" user)))

(define (audit-approver-added change-id approver user)
  "Create audit entry for approver addition"
  (create-audit-entry change-id 'approver-added
                     #:new-value approver
                     #:user user
                     #:details (format #f "Approver ~a added by ~a" approver user)))

(define (audit-risk-score-change change-id old-score new-score user)
  "Create audit entry for risk score change"
  (create-audit-entry change-id 'risk-score-change
                     #:old-value old-score
                     #:new-value new-score
                     #:user user
                     #:details (format #f "Risk score changed from ~a to ~a"
                                       old-score new-score)))

(define (get-audit-trail change-id)
  "Get audit trail for a specific change request"
  (filter (lambda (entry)
            (string=? (audit-entry-change-id entry) change-id))
          audit-trail))

(define (get-all-audit-entries)
  "Get all audit entries"
  (reverse audit-trail))

(define (clear-audit-trail!)
  "Clear the audit trail (for testing)"
  (set! audit-trail '()))

(define (audit-entry->alist entry)
  "Convert audit entry to association list for serialization"
  `((id . ,(audit-entry-id entry))
    (change-id . ,(audit-entry-change-id entry))
    (event-type . ,(audit-entry-event-type entry))
    (old-value . ,(audit-entry-old-value entry))
    (new-value . ,(audit-entry-new-value entry))
    (user . ,(audit-entry-user entry))
    (timestamp . ,(audit-entry-timestamp entry))
    (details . ,(audit-entry-details entry))))