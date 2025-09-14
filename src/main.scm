#!/usr/bin/env guile
!#

;;; Main entry point for the ChangeFlow Web Interface
;;; This starts the HTTP server and web dashboard

(add-to-load-path (string-append (getcwd) "/src"))

(use-modules (web gcf-server))

(define (main args)
  (display "Starting ChangeFlow Web Interface...\n")
  (display "Server will be available at http://localhost:8080\n")
  (display "\n=== Available Endpoints ===\n")
  (display "Dashboard:           http://localhost:8080\n")
  (display "Executive Dashboard: http://localhost:8080/executive\n")
  (display "API:                 http://localhost:8080/api/changes\n")
  (display "Health:              http://localhost:8080/health\n")
  (display "\nPress Ctrl+C to stop the server.\n\n")
  ((@ (web gcf-server) start-web-server)))

;; Run main if executed directly
(main (command-line))