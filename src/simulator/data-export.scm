;;; data-export.scm - Export simulator data to JSON for reproducible demos
;;; Part of the ITIL Process Simulator

(define-module (simulator data-export)
  #:use-module (simulator deployment-pipeline)
  #:use-module (json)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (export-demo-data
            load-demo-data
            export-prs-to-json
            load-prs-from-json
            generate-demo-dataset
            run-demo-with-data))

;; Convert Scheme data to JSON-compatible format
(define (scheme->json-compatible obj)
  "Convert Scheme objects to JSON-compatible format"
  (cond
    ((list? obj)
     (if (and (not (null? obj)) (pair? (car obj)) (symbol? (caar obj)))
         ;; Association list
         (let ((result '()))
           (for-each
            (lambda (pair)
              (let ((key (symbol->string (car pair)))
                    (value (scheme->json-compatible (cdr pair))))
                (set! result (cons (cons key value) result))))
            obj)
           (reverse result))
         ;; Regular list
         (map scheme->json-compatible obj)))
    ((symbol? obj) (symbol->string obj))
    ((procedure? obj) "#<procedure>")
    (else obj)))

;; Convert JSON data back to Scheme format
(define (json->scheme-compatible obj)
  "Convert JSON objects back to Scheme format"
  (cond
    ((list? obj)
     (if (and (not (null? obj)) (pair? (car obj)) (string? (caar obj)))
         ;; JSON object (association list with string keys)
         (map (lambda (pair)
                (cons (string->symbol (car pair))
                      (json->scheme-compatible (cdr pair))))
              obj)
         ;; JSON array
         (map json->scheme-compatible obj)))
    ((string? obj)
     ;; Try to convert back to symbol for known keys
     (case (string->symbol obj)
       ((pending submitted approved scheduled in-progress completed failed cancelled closed) 
        (string->symbol obj))
       ((frontend backend-api backend-db iac-app iac-global)
        (string->symbol obj))
       ((standard normal emergency)
        (string->symbol obj))
       ((low medium high critical)
        (string->symbol obj))
       ((staging production dev test)
        (string->symbol obj))
       (else obj)))
    (else obj)))

;; Export PRs to JSON file
(define (export-prs-to-json prs filename)
  "Export PR list to JSON file"
  (let ((json-data (scheme->json-compatible prs)))
    (call-with-output-file filename
      (lambda (port)
        (put-string port (scm->json-string json-data))))
    (format #t "Exported ~a PRs to ~a~%" (length prs) filename)))

;; Load PRs from JSON file
(define (load-prs-from-json filename)
  "Load PR list from JSON file"
  (if (file-exists? filename)
      (let* ((json-string (call-with-input-file filename get-string-all))
             (json-data (json-string->scm json-string))
             (prs (json->scheme-compatible json-data)))
        (format #t "Loaded ~a PRs from ~a~%" (length prs) filename)
        prs)
      (begin
        (format #t "File ~a not found~%" filename)
        '())))

;; Generate and export complete demo dataset
(define (generate-demo-dataset)
  "Generate complete demo dataset with reproducible results"
  (format #t "🎯 Generating reproducible demo dataset...~%")
  
  ;; Set fixed seed for reproducible results
  (set! *random-state* (seed->random-state 42))
  
  ;; Generate 100 PRs
  (let ((prs (generate-mock-prs)))
    
    ;; Export to JSON
    (export-prs-to-json prs "data/demo-change-requests.json")
    
    ;; Also create subsets for different demo scenarios
    (let ((frontend-prs (filter (lambda (pr) (eq? (assoc-ref pr 'component) 'frontend)) prs))
          (backend-prs (filter (lambda (pr) (member (assoc-ref pr 'component) 
                                                   '(backend-api backend-db))) prs))
          (iac-prs (filter (lambda (pr) (member (assoc-ref pr 'component)
                                               '(iac-app iac-global))) prs))
          (critical-prs (filter (lambda (pr) (eq? (assoc-ref pr 'priority) 'high)) prs)))
      
      (export-prs-to-json (take frontend-prs 10) "data/demo-frontend-only.json")
      (export-prs-to-json (take backend-prs 15) "data/demo-backend-focus.json")
      (export-prs-to-json (take iac-prs 8) "data/demo-infrastructure.json")
      (export-prs-to-json critical-prs "data/demo-high-priority.json")
      
      ;; Create a quick 5-PR demo set
      (export-prs-to-json (take prs 5) "data/demo-quick.json"))
    
    ;; Generate metadata
    (let ((metadata `((generated_at . ,(date->string (current-date) "~Y-~m-~d ~H:~M:~S"))
                      (total_prs . ,(length prs))
                      (components . ((frontend . ,(count-component prs 'frontend))
                                    (backend-api . ,(count-component prs 'backend-api))
                                    (backend-db . ,(count-component prs 'backend-db))
                                    (iac-app . ,(count-component prs 'iac-app))
                                    (iac-global . ,(count-component prs 'iac-global))))
                      (priorities . ((high . ,(count-priority prs 'high))
                                    (medium . ,(count-priority prs 'medium))
                                    (low . ,(count-priority prs 'low))))
                      (change_types . ((standard . ,(count-change-type prs 'standard))
                                      (normal . ,(count-change-type prs 'normal))))
                      (seed . 42)
                      (reproducible . #t))))
      
      (call-with-output-file "data/demo-metadata.json"
        (lambda (port)
          (put-string port (scm->json-string (scheme->json-compatible metadata)))))
      
      (format #t "~%📊 Demo dataset summary:~%")
      (format #t "  Total PRs: ~a~%" (length prs))
      (format #t "  Components: ~a frontend, ~a backend-api, ~a backend-db, ~a iac-app, ~a iac-global~%"
              (count-component prs 'frontend)
              (count-component prs 'backend-api)
              (count-component prs 'backend-db)
              (count-component prs 'iac-app)
              (count-component prs 'iac-global))
      (format #t "  Priorities: ~a high, ~a medium, ~a low~%"
              (count-priority prs 'high)
              (count-priority prs 'medium)
              (count-priority prs 'low))
      
      prs)))

;; Helper functions for counting
(define (count-component prs component)
  (count (lambda (pr) (eq? (assoc-ref pr 'component) component)) prs))

(define (count-priority prs priority)
  (count (lambda (pr) (eq? (assoc-ref pr 'priority) priority)) prs))

(define (count-change-type prs change-type)
  (count (lambda (pr) (eq? (assoc-ref pr 'change-type) change-type)) prs))

;; Export complete demo data (PRs + simulation results)
(define (export-demo-data prs simulation-results filename)
  "Export complete demo data including PRs and simulation results"
  (let ((complete-data `((prs . ,(scheme->json-compatible prs))
                         (simulation . ,(scheme->json-compatible simulation-results))
                         (exported_at . ,(date->string (current-date) "~Y-~m-~d ~H:~M:~S")))))
    (call-with-output-file filename
      (lambda (port)
        (put-string port (scm->json-string complete-data))))
    (format #t "Exported complete demo data to ~a~%" filename)))

;; Load complete demo data
(define (load-demo-data filename)
  "Load complete demo data from JSON file"
  (if (file-exists? filename)
      (let* ((json-string (call-with-input-file filename get-string-all))
             (json-data (json-string->scm json-string))
             (data (json->scheme-compatible json-data)))
        (format #t "Loaded demo data from ~a~%" filename)
        data)
      (begin
        (format #t "File ~a not found~%" filename)
        #f)))

;; Demo runner with pre-generated data
(define (run-demo-with-data json-file num-deployments)
  "Run demo using pre-generated data"
  (let ((prs (load-prs-from-json json-file)))
    (if (not (null? prs))
        (begin
          (format #t "🎬 Running demo with pre-generated data from ~a~%" json-file)
          (format #t "📊 Loaded ~a PRs, processing ~a deployments~%~%" 
                  (length prs) num-deployments)
          (simulate-deployment-pipeline prs num-deployments))
        (format #t "❌ No PRs loaded, cannot run demo~%"))))

;; Module initialization
(format #t "~%=== Data Export Module Loaded ===~%")
(format #t "Ready to export/import demo data!~%")
(format #t "Run (generate-demo-dataset) to create reproducible demo data~%~%")