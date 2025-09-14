(define-module (storage memory)
  #:use-module (models change-request)
  #:use-module (srfi srfi-1)
  #:export (store-change!
            get-change
            list-changes
            update-change!
            delete-change!))

(define change-store (make-hash-table))

(define (store-change! change-request)
  "Store a change request in memory"
  (hash-set! change-store
             (change-request-id change-request)
             change-request)
  change-request)

(define (get-change change-id)
  "Retrieve a change request by ID"
  (hash-ref change-store change-id #f))

(define (list-changes)
  "Get all change requests"
  (hash-map->list (lambda (key value) value) change-store))

(define (update-change! change-id update-proc)
  "Update a change request using the given procedure"
  (let ((change (get-change change-id)))
    (if change
        (let ((updated-change (update-proc change)))
          (hash-set! change-store change-id updated-change)
          updated-change)
        #f)))

(define (delete-change! change-id)
  "Delete a change request"
  (hash-remove! change-store change-id))