(define-module (integrations router)
  #:use-module (integrations github)
  #:use-module (integrations notifications)
  #:use-module (json)
  #:export (route-webhook-event
            handle-event
            register-event-handler
            get-event-handlers))

(define event-handlers (make-hash-table))

(define (register-event-handler event-type handler)
  "Register a handler function for a specific event type"
  (hash-set! event-handlers event-type handler)
  (notify-console
   (format #f "Registered handler for event type: ~a" event-type)
   'debug))

(define (get-event-handlers)
  "Get all registered event handlers"
  event-handlers)

(define (route-webhook-event event-type payload)
  "Route webhook event to appropriate handler"
  (let ((handler (hash-ref event-handlers event-type)))
    (if handler
        (begin
          (notify-console
           (format #f "Routing ~a event to handler" event-type)
           'debug)
          (handler payload))
        (begin
          (notify-console
           (format #f "No handler found for event type: ~a" event-type)
           'warning)
          #f))))

(define (handle-event event-type payload)
  "Main event handling function with fallback to built-in handlers"
  (cond
    ((route-webhook-event event-type payload)
     (notify-console
      (format #f "Event ~a handled by custom handler" event-type)
      'success))

    ((string=? event-type "pull_request")
     (notify-console "Using built-in PR handler" 'info)
     (process-pull-request payload))

    ((string=? event-type "push")
     (notify-console "Using built-in push handler" 'info)
     (process-push payload))

    (else
     (notify-console
      (format #f "Unhandled event type: ~a" event-type)
      'warning))))

(register-event-handler "pull_request" process-pull-request)
(register-event-handler "push" process-push)