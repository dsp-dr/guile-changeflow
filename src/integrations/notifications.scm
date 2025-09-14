(define-module (integrations notifications)
  #:use-module (srfi srfi-19)
  #:export (notify-console
            notify-change-created
            notify-risk-assessed
            notify-status-changed))

(define* (notify-console message #:optional (level 'info))
  "Print notification to console with timestamp and level"
  (let* ((now (current-time))
         (timestamp (date->string (time-utc->date now) "~Y-~m-~d ~H:~M:~S"))
         (prefix (case level
                   ((error) "❌ ERROR")
                   ((success) "✅ SUCCESS")
                   ((warning) "⚠️  WARNING")
                   ((info) "ℹ️  INFO")
                   ((debug) "🔍 DEBUG")
                   ((startup) "🚀 STARTUP")
                   (else "📢 NOTIFICATION"))))

    (format #t "[~a] ~a: ~a\n" timestamp prefix message)
    (force-output)))

(define (notify-change-created change-id title)
  (notify-console
   (format #f "New change request created: ~a - ~a" change-id title)
   'success))

(define (notify-risk-assessed change-id risk-score category)
  (let ((emoji (cond
                 ((eq? category 'low) "🟢")
                 ((eq? category 'medium) "🟡")
                 ((eq? category 'high) "🟠")
                 ((eq? category 'critical) "🔴")
                 (else "⚪"))))
    (notify-console
     (format #f "~a Risk assessed for ~a: ~a (~a)"
            emoji change-id risk-score category)
     'info)))

(define (notify-status-changed change-id old-status new-status)
  (notify-console
   (format #f "Status change for ~a: ~a → ~a"
          change-id old-status new-status)
   'info))