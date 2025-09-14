(define-module (main)
  #:use-module (webhooks server)
  #:use-module (integrations notifications)
  #:use-module (integrations mock)
  #:use-module (integrations router)
  #:export (main))

(define (display-banner)
  "Display startup banner"
  (display "
╔═══════════════════════════════════════════════╗
║          Guile ChangeFlow Integrations        ║
║               GitHub Webhooks                 ║
╚═══════════════════════════════════════════════╝
")
  (notify-console "Guile ChangeFlow Integrations starting up..." 'startup))

(define (display-help)
  "Display help information"
  (display "
Guile ChangeFlow Integrations - Usage:

  guile -s src/main.scm [OPTION]

Options:
  --server      Start the webhook server (default)
  --test        Run mock payload test
  --simulate    Simulate a PR event
  --help        Show this help

Endpoints:
  http://localhost:8082/webhooks/github  - GitHub webhook endpoint
  http://localhost:8082/webhooks/test    - Test endpoint
  http://localhost:8082/health           - Health check

"))

(define (main args)
  "Main entry point"
  (display-banner)

  (cond
    ((or (null? args) (member "--server" args))
     (notify-console "Starting webhook server mode..." 'info)
     (start-webhook-server))

    ((member "--test" args)
     (notify-console "Running mock payload test..." 'info)
     (test-webhook-with-mock))

    ((member "--simulate" args)
     (notify-console "Simulating GitHub PR event..." 'info)
     (simulate-github-pr))

    ((member "--help" args)
     (display-help))

    (else
     (notify-console "Unknown option. Use --help for usage information." 'error)
     (display-help))))

(when (defined? 'command-line)
  (main (command-line)))