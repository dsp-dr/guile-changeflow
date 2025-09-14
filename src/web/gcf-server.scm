(define-module (web gcf-server)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (web api)
  #:use-module (web dashboard)
  #:use-module (web executive-dashboard)
  #:export (start-web-server))

(define (handler request request-body)
  "Main request handler that routes requests to appropriate handlers"
  (let* ((uri (request-uri request))
         (path (uri-path uri))
         (method (request-method request)))

    (display (format #f "~a ~a\n" method path))

    (cond
      ((string=? path "/")
       (dashboard-handler))
      ((string=? path "/executive")
       (executive-dashboard-handler))
      ((string=? path "/health")
       (health-handler))
      ((string=? path "/api/changes")
       (api-changes-handler))
      ((string-prefix? "/api/" path)
       (api-handler request request-body))
      (else
       (not-found-handler)))))

(define (health-handler)
  "Health check endpoint"
  (values (build-response
           #:code 200
           #:headers '((content-type . (application/json))
                       (access-control-allow-origin . "*")))
          "{\"status\":\"healthy\",\"service\":\"web-interface\",\"timestamp\":\"2025-09-13T17:41:00Z\"}"))

(define (not-found-handler)
  "404 handler"
  (values (build-response
           #:code 404
           #:headers '((content-type . (text/plain))))
          "Not Found"))

(define (api-handler request body)
  "Generic API handler for unknown endpoints"
  (values (build-response
           #:code 404
           #:headers '((content-type . (application/json))
                       (access-control-allow-origin . "*")))
          "{\"error\":\"API endpoint not found\"}"))

(define (start-web-server)
  "Start the HTTP server on port 8080"
  (display "Starting web server on port 8080...\n")
  (display "Available endpoints:\n")
  (display "  GET /           - Dashboard\n")
  (display "  GET /executive  - Executive Dashboard (ROI & Metrics)\n")
  (display "  GET /health     - Health check\n")
  (display "  GET /api/changes - Changes API\n")
  (display "\n")
  (run-server handler 'http '(#:port 8080)))