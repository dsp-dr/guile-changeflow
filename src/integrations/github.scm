(define-module (integrations github)
  #:use-module (models change-request)
  #:use-module (storage memory)
  #:use-module (integrations notifications)
  #:use-module (json)
  #:use-module (srfi srfi-19)
  #:export (process-pull-request
            process-push
            create-change-from-pr))

(define (process-pull-request payload)
  "Process GitHub pull request event"
  (let* ((action (assoc-ref payload 'action))
         (pr (assoc-ref payload 'pull_request))
         (title (assoc-ref pr 'title))
         (body (assoc-ref pr 'body))
         (number (assoc-ref pr 'number))
         (author (assoc-ref (assoc-ref pr 'user) 'login)))

    (when (string=? action "opened")
      (let* ((change-id (format #f "CHG-PR-~a" number))
             (description (format #f "PR #~a by @~a\n\n~a"
                                 number author (or body "No description")))
             (change (make-change-request
                      change-id
                      title
                      description
                      25  ; Default risk score for PRs
                      'submitted
                      (current-time)
                      (current-time))))

        (store-change! change)

        (notify-console
         (format #f "Created change ~a from PR #~a: ~a"
                change-id number title)
         'success)

        change))))

(define (process-push payload)
  "Process GitHub push event"
  (let* ((ref (assoc-ref payload 'ref))
         (commits (assoc-ref payload 'commits))
         (pusher (assoc-ref (assoc-ref payload 'pusher) 'name)))

    (notify-console
     (format #f "Push to ~a by ~a (~a commits)"
            ref pusher (length commits))
     'info)))

(define (create-change-from-pr pr-number pr-title pr-body pr-author)
  "Helper function to create change request from PR data"
  (let* ((change-id (format #f "CHG-PR-~a" pr-number))
         (description (format #f "PR #~a by @~a\n\n~a"
                             pr-number pr-author (or pr-body "No description")))
         (change (make-change-request
                  change-id
                  pr-title
                  description
                  25
                  'submitted
                  (current-time)
                  (current-time))))

    (store-change! change)
    (notify-change-created change-id pr-title)
    change))