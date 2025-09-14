(define-module (storage memory)
  #:use-module (models change-request)
  #:use-module (models approval)
  #:use-module (models audit)
  #:use-module (models state-machine)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (init-storage!
            store-change!
            get-change
            get-all-changes
            update-change!
            delete-change!
            find-changes-by-status
            find-changes-by-submitter
            change-exists?
            get-change-count
            clear-storage!
            store-approval!
            get-approvals-for-change
            get-all-approvals
            storage-stats))

(define changes-table (make-hash-table))
(define approvals-table (make-hash-table))
(define next-change-id 1)

(define (init-storage!)
  "Initialize the storage system"
  (hash-clear! changes-table)
  (hash-clear! approvals-table)
  (set! next-change-id 1)
  (clear-audit-trail!)
  #t)

(define (generate-change-id)
  "Generate a unique change request ID"
  (let ((id (string-append "CHG-" (string-pad (number->string next-change-id) 6 #\0))))
    (set! next-change-id (+ next-change-id 1))
    id))

(define (store-change! change)
  "Store a change request in memory"
  (let ((id (change-request-id change)))
    (hash-set! changes-table id change)
    (audit-state-change id #f (change-request-status change) "system")
    id))

(define (get-change id)
  "Retrieve a change request by ID"
  (hash-ref changes-table id #f))

(define (get-all-changes)
  "Retrieve all change requests"
  (hash-map->list (lambda (k v) v) changes-table))

(define (update-change! id updates-proc)
  "Update a change request using the provided procedure"
  (let ((change (get-change id)))
    (if change
        (let ((old-status (change-request-status change)))
          (updates-proc change)
          (set-change-request-updated-at! change (current-time))
          (let ((new-status (change-request-status change)))
            (unless (eq? old-status new-status)
              (audit-state-change id old-status new-status "system")))
          (hash-set! changes-table id change)
          change)
        (error "Change request not found" id))))

(define (delete-change! id)
  "Delete a change request"
  (let ((change (get-change id)))
    (if change
        (begin
          (hash-remove! changes-table id)
          (audit-state-change id (change-request-status change) 'deleted "system")
          #t)
        #f)))

(define (find-changes-by-status status)
  "Find all change requests with the specified status"
  (filter (lambda (change)
            (eq? (change-request-status change) status))
          (get-all-changes)))

(define (find-changes-by-submitter submitter)
  "Find all change requests submitted by the specified user"
  (filter (lambda (change)
            (equal? (change-request-submitter change) submitter))
          (get-all-changes)))

(define (change-exists? id)
  "Check if a change request exists"
  (not (not (hash-ref changes-table id #f))))

(define (get-change-count)
  "Get the total number of change requests"
  (hash-count (lambda (k v) #t) changes-table))

(define (clear-storage!)
  "Clear all storage (for testing)"
  (init-storage!))

(define (store-approval! approval)
  "Store an approval in memory"
  (let ((change-id (approval-change-id approval))
        (approvals (hash-ref approvals-table change-id '())))
    (hash-set! approvals-table change-id (cons approval approvals))
    approval))

(define (get-approvals-for-change change-id)
  "Get all approvals for a specific change request"
  (reverse (hash-ref approvals-table change-id '())))

(define (get-all-approvals)
  "Get all approvals in the system"
  (apply append (hash-map->list (lambda (k v) v) approvals-table)))

(define (create-and-store-change! title description
                                 #:key
                                 (risk-score 0)
                                 (status 'submitted)
                                 (submitter #f)
                                 (impact-level 'medium)
                                 (urgency 'normal)
                                 (category 'standard))
  "Create a new change request and store it"
  (let* ((id (generate-change-id))
         (change (create-change-request id title description
                                       #:risk-score risk-score
                                       #:status status
                                       #:submitter submitter
                                       #:impact-level impact-level
                                       #:urgency urgency
                                       #:category category)))
    (store-change! change)
    change))

(define (storage-stats)
  "Get storage statistics"
  `((total-changes . ,(get-change-count))
    (total-approvals . ,(length (get-all-approvals)))
    (total-audit-entries . ,(length (get-all-audit-entries)))
    (changes-by-status . ,(map (lambda (status)
                                (cons status
                                      (length (find-changes-by-status status))))
                              (get-all-states)))))

(define (transition-change! change-id new-state user)
  "Transition a change to a new state with audit logging"
  (let ((change (get-change change-id)))
    (if change
        (let ((old-state (change-request-status change)))
          (if (valid-transition? old-state new-state)
              (begin
                (set-change-request-status! change new-state)
                (set-change-request-updated-at! change (current-time))
                (audit-state-change change-id old-state new-state user)
                (hash-set! changes-table change-id change)
                change)
              (error "Invalid state transition" change-id old-state new-state)))
        (error "Change request not found" change-id))))