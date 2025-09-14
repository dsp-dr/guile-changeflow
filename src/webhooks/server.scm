(define-module (webhooks server)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (json)
  #:use-module (integrations github)
  #:use-module (integrations notifications)
  #:export (start-webhook-server))

(define (handler request body)
  (let ((path (uri-path (request-uri request)))
        (headers (request-headers request)))
    (cond
      ((string=? path "/webhooks/github")
       (handle-github-webhook request body))
      ((string=? path "/webhooks/test")
       (handle-test-webhook))
      ((string=? path "/health")
       (health-handler))
      (else
       (not-found-handler)))))

(define (handle-github-webhook request body)
  (let* ((event-type (assoc-ref (request-headers request) 'x-github-event))
         (payload (json-string->scm (utf8->string body))))

    (notify-console
     (format #f "GitHub Event: ~a" event-type)
     'info)

    (cond
      ((string=? event-type "pull_request")
       (process-pull-request payload))
      ((string=? event-type "push")
       (process-push payload))
      (else
       (notify-console
        (format #f "Ignoring event type: ~a" event-type)
        'debug)))

    (values '((content-type . (application/json)))
            "{\"status\":\"received\"}")))

(define (handle-test-webhook)
  (notify-console "Test webhook endpoint hit" 'debug)
  (values '((content-type . (application/json)))
          "{\"status\":\"ok\",\"message\":\"Test endpoint working\"}"))

(define (health-handler)
  (values '((content-type . (application/json)))
          "{\"status\":\"healthy\",\"service\":\"webhook-server\"}"))

(define (not-found-handler)
  (values (build-response #:code 404
                          #:headers '((content-type . (application/json))))
          "{\"error\":\"Endpoint not found\"}"))

(define (start-webhook-server)
  (display "Starting webhook server on port 8082...\n")
  (notify-console "Webhook server starting..." 'startup)
  (run-server handler 'http '(#:port 8082)))