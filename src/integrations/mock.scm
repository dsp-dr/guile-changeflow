(define-module (integrations mock)
  #:use-module (json)
  #:use-module (integrations github)
  #:use-module (integrations notifications)
  #:export (generate-pr-payload
            generate-push-payload
            test-webhook-with-mock
            simulate-github-pr))

(define (generate-pr-payload)
  "Generate mock GitHub PR payload for testing"
  `((action . "opened")
    (number . ,(+ 100 (random 900)))
    (pull_request . ((title . "Fix critical security vulnerability")
                     (body . "This PR fixes CVE-2024-12345 in the authentication system")
                     (number . 423)
                     (state . "open")
                     (user . ((login . "developer-bot")
                             (type . "User")))
                     (head . ((ref . "fix/security-patch")))
                     (base . ((ref . "main")))))
    (repository . ((name . "guile-changeflow")
                   (full_name . "dsp-dr/guile-changeflow")))))

(define (generate-push-payload)
  "Generate mock GitHub push payload"
  `((ref . "refs/heads/main")
    (commits . #(((id . "abc123")
                  (message . "Update dependencies")
                  (author . ((name . "Test User"))))))
    (pusher . ((name . "test-user")))
    (repository . ((name . "guile-changeflow")))))

(define (test-webhook-with-mock)
  "Test webhook processing with mock data"
  (let ((pr-json (scm->json-string (generate-pr-payload))))
    (display "Testing with mock PR payload...\n")
    (display pr-json)
    (newline)
    #t))

(define (simulate-github-pr)
  "Simulate a GitHub PR event for testing"
  (let* ((payload (generate-pr-payload))
         (pr (assoc-ref payload 'pull_request))
         (number (assoc-ref pr 'number))
         (title (assoc-ref pr 'title))
         (body (assoc-ref pr 'body))
         (author (assoc-ref (assoc-ref pr 'user) 'login)))

    (notify-console "Simulating GitHub PR event..." 'debug)
    (process-pull-request payload)

    (notify-console
     (format #f "Mock PR simulation complete: #~a '~a'" number title)
     'success)))