;;; Main entry point for Guile ChangeFlow
;;; Combines Risk Engine and Web Interface functionality

(define-module (main)
  #:use-module (risk calculator)
  #:use-module (risk categories)
  #:use-module (risk freeze)
  #:use-module (risk factors)
  #:use-module (web gcf-server)
  #:export (assess-risk
            assess-change-request
            main

            ;; Risk exports
            calculate-base-risk
            calculate-system-risk
            calculate-time-risk
            categorize-risk
            get-risk-color
            get-approval-requirement
            risk-category-info
            in-freeze-period?
            get-next-window
            check-blackout-dates
            get-freeze-risk-modifier
            risk-factors))

;; Risk Engine functionality
(define (assess-change-request request-json)
  "Main entry point for risk assessment from MCP server
   Expects: ((title . \"string\") (description . \"string\") (systems . (list)))
   Returns: Complete risk assessment with score, category, and recommendations"
  (let* ((title (or (assoc-ref request-json 'title) ""))
         (description (or (assoc-ref request-json 'description) ""))
         (systems (or (assoc-ref request-json 'systems) '()))
         (assessment (assess-risk title description systems)))
    assessment))

;; Web Interface functionality
(add-to-load-path (string-append (getcwd) "/src"))

(define (main args)
  (cond
    ;; Run as web server
    ((member "--web" args)
     (display "Starting ChangeFlow Web Interface...\n")
     (display "Server will be available at http://localhost:8080\n")
     (display "\n=== Available Endpoints ===\n")
     (display "Dashboard:           http://localhost:8080\n")
     (display "Executive Dashboard: http://localhost:8080/executive\n")
     (display "API:                 http://localhost:8080/api/changes\n")
     (display "Health:              http://localhost:8080/health\n")
     (display "\nPress Ctrl+C to stop the server.\n\n")
     ((@ (web gcf-server) start-web-server)))

    ;; Run as risk assessment CLI
    ((member "--risk" args)
     (display "Risk Assessment Engine\n")
     (display "Enter change title: ")
     (let ((title (read-line)))
       (display "Enter change description: ")
       (let ((description (read-line)))
         (let ((result (assess-risk title description '())))
           (display (format #f "Risk Score: ~a\n" (assoc-ref result 'score)))
           (display (format #f "Risk Level: ~a\n" (assoc-ref result 'level)))))))

    ;; Default: show usage
    (else
     (display "Guile ChangeFlow - ITIL 4 Change Management System\n")
     (display "\nUsage:\n")
     (display "  guile -s src/main.scm --web   # Start web server\n")
     (display "  guile -s src/main.scm --risk  # Run risk assessment\n")
     (display "\n"))))

;; Run main if executed directly
(when (not (null? (command-line)))
  (main (command-line)))