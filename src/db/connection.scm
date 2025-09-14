;;; SQLite Database Connection Module
;;; Provides connection pooling and transaction management

(define-module (db connection)
  #:use-module (sqlite3)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (get-db-connection
            with-db-connection
            with-transaction
            execute-query
            execute-update
            execute-insert
            prepare-statement
            close-db-connection
            init-database!))

(define *db-path* (or (getenv "GCF_DB_PATH") "changeflow.db"))
(define *connection-pool* '())
(define *pool-mutex* (make-mutex))
(define *max-connections* 5)

(define (create-connection)
  "Create a new database connection with proper settings"
  (let ((db (sqlite-open *db-path*)))
    ;; Configure database for production use
    (sqlite-exec db "PRAGMA foreign_keys = ON")
    (sqlite-exec db "PRAGMA journal_mode = WAL")
    (sqlite-exec db "PRAGMA synchronous = NORMAL")
    (sqlite-exec db "PRAGMA temp_store = MEMORY")
    (sqlite-exec db "PRAGMA mmap_size = 30000000000")
    db))

(define (get-db-connection)
  "Get a database connection from the pool"
  (with-mutex *pool-mutex*
    (if (null? *connection-pool*)
        (create-connection)
        (let ((conn (car *connection-pool*)))
          (set! *connection-pool* (cdr *connection-pool*))
          conn))))

(define (return-connection conn)
  "Return a connection to the pool"
  (with-mutex *pool-mutex*
    (when (< (length *connection-pool*) *max-connections*)
      (set! *connection-pool* (cons conn *connection-pool*)))))

(define (with-db-connection proc)
  "Execute procedure with a database connection"
  (let ((conn (get-db-connection)))
    (catch #t
      (lambda ()
        (let ((result (proc conn)))
          (return-connection conn)
          result))
      (lambda (key . args)
        (return-connection conn)
        (apply throw key args)))))

(define (with-transaction conn proc)
  "Execute procedure within a database transaction"
  (sqlite-exec conn "BEGIN TRANSACTION")
  (catch #t
    (lambda ()
      (let ((result (proc conn)))
        (sqlite-exec conn "COMMIT")
        result))
    (lambda (key . args)
      (sqlite-exec conn "ROLLBACK")
      (apply throw key args))))

(define (execute-query conn sql . params)
  "Execute a SELECT query and return all results"
  (let ((stmt (sqlite-prepare conn sql)))
    (let loop ((i 1) (ps params))
      (unless (null? ps)
        (sqlite-bind stmt i (car ps))
        (loop (+ i 1) (cdr ps))))

    (let loop ((row (sqlite-step stmt))
               (results '()))
      (if row
          (loop (sqlite-step stmt)
                (cons (vector->list row) results))
          (begin
            (sqlite-finalize stmt)
            (reverse results))))))

(define (execute-update conn sql . params)
  "Execute an UPDATE/DELETE query and return affected rows"
  (let ((stmt (sqlite-prepare conn sql)))
    (let loop ((i 1) (ps params))
      (unless (null? ps)
        (sqlite-bind stmt i (car ps))
        (loop (+ i 1) (cdr ps))))

    (sqlite-step stmt)
    (sqlite-finalize stmt)
    (sqlite-changes conn)))

(define (execute-insert conn sql . params)
  "Execute an INSERT query and return the last inserted ID"
  (let ((stmt (sqlite-prepare conn sql)))
    (let loop ((i 1) (ps params))
      (unless (null? ps)
        (sqlite-bind stmt i (car ps))
        (loop (+ i 1) (cdr ps))))

    (sqlite-step stmt)
    (sqlite-finalize stmt)
    (sqlite-last-insert-rowid conn)))

(define (prepare-statement conn sql)
  "Prepare a statement for repeated execution"
  (sqlite-prepare conn sql))

(define (close-db-connection conn)
  "Close a database connection"
  (sqlite-close conn))

(define (init-database!)
  "Initialize the database with migrations"
  (system* "guile" "migrations/migrate.scm")
  (format #t "Database initialized at ~a~%" *db-path*))