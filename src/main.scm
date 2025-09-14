#!/usr/bin/env guile
!#

;;; Main entry point for the ChangeFlow Web Interface
;;; This starts the HTTP server and web dashboard

(add-to-load-path (dirname (current-filename)))

(use-modules (web server))

(define (main args)
  (display "Starting ChangeFlow Web Interface...\n")
  (display "Server will be available at http://localhost:8080\n")
  (display "Dashboard: http://localhost:8080\n")
  (display "API: http://localhost:8080/api/changes\n")
  (display "Health: http://localhost:8080/health\n")
  (display "\nPress Ctrl+C to stop the server.\n\n")
  ((@ (web server) start-web-server)))

;; Run main if executed directly
(when (equal? (car (command-line)) (current-filename))
  (main (command-line)))