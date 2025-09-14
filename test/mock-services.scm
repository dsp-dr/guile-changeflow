#!/usr/bin/env guile -s
!#

;;; Mock Services for Integration Testing
;;; Simulates MCP, Web, and Webhook servers for testing

(define-module (test mock-services)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (ice-9 format)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-19)
  #:export (start-mock-services))

;; In-memory storage for changes
(define changes '())
(define change-counter 0)
(define mutex (make-mutex))

;; Generate change ID
(define (generate-change-id)
  (with-mutex mutex
    (set! change-counter (+ change-counter 1))
    (format #f "CHG-2025-~3,'0d" change-counter)))

;; Calculate risk score
(define (calculate-risk title description)
  (let ((score 10))
    (when (and description (string-contains-ci description "production"))
      (set! score (+ score 40)))
    (when (and description (string-contains-ci description "security"))
      (set! score (+ score 30)))
    (when (and description (string-contains-ci description "payment"))
      (set! score (+ score 20)))
    (when (and description (string-contains-ci description "auth"))
      (set! score (+ score 15)))
    (min score 100)))

(define (string-contains-ci str substr)
  (and str substr
       (string-contains (string-downcase str) (string-downcase substr))))

;; Get risk category
(define (get-risk-category score)
  (cond
    ((< score 30) "low")
    ((< score 70) "medium")
    (else "high")))

;; Create change from request
(define (create-change title description)
  (let* ((id (generate-change-id))
         (risk-score (calculate-risk title description))
         (risk-category (get-risk-category risk-score))
         (change `((id . ,id)
                  (title . ,title)
                  (description . ,description)
                  (risk_score . ,risk-score)
                  (risk_category . ,risk-category)
                  (status . "submitted")
                  (created_at . ,(date->string (current-date) "~Y-~m-~dT~H:~M:~SZ")))))
    (with-mutex mutex
      (set! changes (cons change changes)))
    change))

;; MCP Server Handler (port 8081)
(define (mcp-handler request body)
  (let ((path (uri-path (request-uri request))))
    (cond
      ;; MCP Discovery
      ((string=? path "/.well-known/mcp")
       (values '((content-type . (application/json)))
               "{\"mcp_version\":\"1.0.0\",\"server_name\":\"mock-changeflow\",\"capabilities\":{\"tools\":true}}"))

      ;; List tools
      ((string=? path "/tools")
       (values '((content-type . (application/json)))
               "[{\"name\":\"create_change_request\",\"description\":\"Create change\"}]"))

      ;; Create change tool
      ((string=? path "/tools/create_change_request/invoke")
       (let* ((body-str (if body (utf8->string body) "{}"))
              (title "Test Change")
              (description "Test Description"))
         ;; Simple JSON parsing (extract title/description)
         (when (string-contains body-str "\"title\"")
           (let ((match (string-match "\"title\"[[:space:]]*:[[:space:]]*\"([^\"]+)\"" body-str)))
             (when match
               (set! title (match:substring match 1)))))
         (when (string-contains body-str "\"description\"")
           (let ((match (string-match "\"description\"[[:space:]]*:[[:space:]]*\"([^\"]+)\"" body-str)))
             (when match
               (set! description (match:substring match 1)))))

         (let ((change (create-change title description)))
           (values '((content-type . (application/json)))
                   (format #f "{\"id\":\"~a\",\"risk_score\":~a,\"status\":\"created\"}"
                          (assoc-ref change 'id)
                          (assoc-ref change 'risk_score))))))

      (else
       (values (build-response #:code 404) "Not found")))))

;; Web Server Handler (port 8080)
(define (web-handler request body)
  (let ((path (uri-path (request-uri request)))
        (method (request-method request)))
    (cond
      ;; Health check
      ((string=? path "/health")
       (values '((content-type . (text/plain)))
               "OK"))

      ;; List changes API
      ((and (string=? path "/api/changes") (eq? method 'GET))
       (let ((json-changes
              (string-join
               (map (lambda (c)
                      (format #f "{\"id\":\"~a\",\"title\":\"~a\",\"risk_score\":~a,\"risk_category\":\"~a\",\"status\":\"~a\"}"
                             (assoc-ref c 'id)
                             (assoc-ref c 'title)
                             (assoc-ref c 'risk_score)
                             (assoc-ref c 'risk_category)
                             (assoc-ref c 'status)))
                    changes)
               ",")))
         (values '((content-type . (application/json)))
                 (format #f "[~a]" json-changes))))

      ;; Create change API
      ((and (string=? path "/api/changes") (eq? method 'POST))
       (let* ((body-str (if body (utf8->string body) "{}"))
              (title "API Change")
              (description "Created via API"))
         ;; Extract title/description from body
         (when (string-contains body-str "\"title\"")
           (let ((match (string-match "\"title\"[[:space:]]*:[[:space:]]*\"([^\"]+)\"" body-str)))
             (when match
               (set! title (match:substring match 1)))))
         (when (string-contains body-str "\"description\"")
           (let ((match (string-match "\"description\"[[:space:]]*:[[:space:]]*\"([^\"]+)\"" body-str)))
             (when match
               (set! description (match:substring match 1)))))

         (let ((change (create-change title description)))
           (values '((content-type . (application/json)))
                   (format #f "{\"id\":\"~a\",\"status\":\"created\"}"
                          (assoc-ref change 'id))))))

      (else
       (values (build-response #:code 404) "Not found")))))

;; Webhook Server Handler (port 8082)
(define (webhook-handler request body)
  (let ((path (uri-path (request-uri request))))
    (cond
      ;; Health check
      ((string=? path "/health")
       (values '((content-type . (text/plain)))
               "OK"))

      ;; GitHub webhook
      ((string=? path "/webhooks/github")
       (let* ((body-str (if body (utf8->string body) "{}"))
              (title "GitHub PR")
              (description "From webhook"))
         ;; Extract PR title if present
         (when (string-contains body-str "\"title\"")
           (let ((match (string-match "\"title\"[[:space:]]*:[[:space:]]*\"([^\"]+)\"" body-str)))
             (when match
               (set! title (match:substring match 1)))))

         (create-change title description)
         (values '((content-type . (application/json)))
                 "{\"status\":\"received\"}")))

      (else
       (values (build-response #:code 404) "Not found")))))

;; Start all mock services
(define (start-mock-services)
  (format #t "Starting mock services...~%")

  ;; Start MCP server on 8081
  (let ((mcp-thread
         (make-thread
          (lambda ()
            (format #t "Mock MCP server on port 8081~%")
            (run-server mcp-handler 'http '(#:port 8081))))))

    ;; Start Web server on 8080
    (let ((web-thread
           (make-thread
            (lambda ()
              (format #t "Mock Web server on port 8080~%")
              (run-server web-handler 'http '(#:port 8080))))))

      ;; Start Webhook server on 8082
      (format #t "Mock Webhook server on port 8082~%")
      (run-server webhook-handler 'http '(#:port 8082)))))

;; Run if executed directly
(when (batch-mode?)
  (start-mock-services))