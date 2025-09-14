#!/usr/bin/env guile
!#

;;; Database Migration Runner for Guile ChangeFlow
;;; Applies SQL migrations in order with version tracking

(use-modules (sqlite3)
             (ice-9 ftw)
             (ice-9 textual-ports)
             (ice-9 format)
             (ice-9 regex)
             (srfi srfi-1))

(define *db-path* "changeflow.db")
(define *migrations-dir* "migrations")

(define (create-migrations-table db)
  "Create the migrations tracking table if it doesn't exist"
  (sqlite-exec db "
    CREATE TABLE IF NOT EXISTS schema_migrations (
      version TEXT PRIMARY KEY,
      applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      checksum TEXT
    )"))

(define (get-applied-migrations db)
  "Get list of already applied migrations"
  (let ((stmt (sqlite-prepare db "SELECT version FROM schema_migrations ORDER BY version")))
    (let loop ((row (sqlite-step stmt))
               (versions '()))
      (if row
          (loop (sqlite-step stmt)
                (cons (vector-ref row 0) versions))
          (begin
            (sqlite-finalize stmt)
            (reverse versions))))))

(define (calculate-checksum content)
  "Calculate a simple checksum for migration content"
  (let ((sum 0))
    (string-for-each
     (lambda (ch)
       (set! sum (modulo (+ sum (char->integer ch)) 65536)))
     content)
    (format #f "~4,'0x" sum)))

(define (read-migration-file filepath)
  "Read entire migration file content"
  (call-with-input-file filepath get-string-all))

(define (get-migration-files)
  "Get all .sql migration files sorted by version"
  (let ((files '()))
    (ftw *migrations-dir*
         (lambda (filename statinfo flag)
           (when (and (eq? flag 'regular)
                      (string-suffix? ".sql" filename))
             (set! files (cons filename files)))
           #t))
    (sort files string<?)))

(define (extract-version filename)
  "Extract version number from migration filename"
  (let ((match (string-match "([0-9]+)" (basename filename))))
    (if match
        (match:substring match 1)
        #f)))

(define (apply-migration db filepath)
  "Apply a single migration file"
  (let* ((content (read-migration-file filepath))
         (version (extract-version filepath))
         (checksum (calculate-checksum content)))

    (format #t "Applying migration ~a: ~a~%" version (basename filepath))

    ;; Begin transaction
    (sqlite-exec db "BEGIN TRANSACTION")

    (catch #t
      (lambda ()
        ;; Execute migration SQL
        (for-each
         (lambda (statement)
           (when (and (not (string-null? statement))
                      (not (string-every char-whitespace? statement)))
             (sqlite-exec db statement)))
         (string-split content #\;))

        ;; Record migration
        (let ((stmt (sqlite-prepare db
                      "INSERT INTO schema_migrations (version, checksum) VALUES (?, ?)")))
          (sqlite-bind stmt 1 version)
          (sqlite-bind stmt 2 checksum)
          (sqlite-step stmt)
          (sqlite-finalize stmt))

        ;; Commit transaction
        (sqlite-exec db "COMMIT")
        (format #t "  ✓ Migration ~a applied successfully~%" version))

      (lambda (key . args)
        ;; Rollback on error
        (sqlite-exec db "ROLLBACK")
        (format #t "  ✗ Migration ~a failed: ~a~%" version args)
        (error "Migration failed" version args)))))

(define (run-migrations)
  "Run all pending migrations"
  (let ((db (sqlite-open *db-path*)))

    ;; Enable foreign keys and WAL mode
    (sqlite-exec db "PRAGMA foreign_keys = ON")
    (sqlite-exec db "PRAGMA journal_mode = WAL")

    ;; Create migrations table
    (create-migrations-table db)

    ;; Get applied migrations
    (let ((applied (get-applied-migrations db))
          (migration-files (get-migration-files)))

      (format #t "Found ~a migration files~%" (length migration-files))
      (format #t "Already applied: ~a~%" applied)

      ;; Apply pending migrations
      (for-each
       (lambda (filepath)
         (let ((version (extract-version filepath)))
           (unless (member version applied)
             (apply-migration db filepath))))
       migration-files)

      (format #t "~%All migrations completed successfully!~%"))

    (sqlite-close db)))

(define (rollback-migration db version)
  "Rollback a specific migration (if down script exists)"
  (let* ((down-file (format #f "~a/~a_down.sql" *migrations-dir* version))
         (exists? (file-exists? down-file)))

    (if exists?
        (begin
          (format #t "Rolling back migration ~a~%" version)
          (sqlite-exec db "BEGIN TRANSACTION")

          (catch #t
            (lambda ()
              ;; Execute rollback SQL
              (let ((content (read-migration-file down-file)))
                (for-each
                 (lambda (statement)
                   (when (not (string-null? statement))
                     (sqlite-exec db statement)))
                 (string-split content #\;)))

              ;; Remove migration record
              (let ((stmt (sqlite-prepare db
                            "DELETE FROM schema_migrations WHERE version = ?")))
                (sqlite-bind stmt 1 version)
                (sqlite-step stmt)
                (sqlite-finalize stmt))

              (sqlite-exec db "COMMIT")
              (format #t "  ✓ Migration ~a rolled back~%" version))

            (lambda (key . args)
              (sqlite-exec db "ROLLBACK")
              (format #t "  ✗ Rollback failed: ~a~%" args)
              (error "Rollback failed" version args))))

        (format #t "No rollback script found for version ~a~%" version))))

(define (show-status)
  "Show migration status"
  (let ((db (sqlite-open *db-path*)))
    (create-migrations-table db)

    (format #t "~%Migration Status:~%")
    (format #t "================~%")

    (let ((stmt (sqlite-prepare db
                  "SELECT version, applied_at, checksum FROM schema_migrations ORDER BY version")))
      (let loop ((row (sqlite-step stmt)))
        (when row
          (format #t "~a | ~a | ~a~%"
                  (vector-ref row 0)
                  (vector-ref row 1)
                  (vector-ref row 2))
          (loop (sqlite-step stmt))))
      (sqlite-finalize stmt))

    (sqlite-close db)))

;; Main entry point
(define (main args)
  (cond
   ((null? (cdr args))
    (run-migrations))
   ((string=? (cadr args) "status")
    (show-status))
   ((string=? (cadr args) "rollback")
    (if (null? (cddr args))
        (format #t "Usage: ~a rollback VERSION~%" (car args))
        (let ((db (sqlite-open *db-path*)))
          (rollback-migration db (caddr args))
          (sqlite-close db))))
   (else
    (format #t "Usage: ~a [status|rollback VERSION]~%" (car args)))))

(main (command-line))