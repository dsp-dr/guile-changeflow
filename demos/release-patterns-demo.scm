#!/usr/bin/env guile3
!#

;;; Release Patterns Interactive Demo
;;; Run with: guile3 demos/release-patterns-demo.scm

(add-to-load-path (string-append (dirname (current-filename)) "/../src"))

(use-modules (simulator release-environment-simulator)
             (ice-9 format))

(define (display-menu)
  (format #t "~%╔════════════════════════════════════════════════════════╗~%")
  (format #t "║         RELEASE PATTERN INTERACTIVE DEMO              ║~%")
  (format #t "╚════════════════════════════════════════════════════════╝~%")
  (format #t "~%Choose a deployment pattern to simulate:~%")
  (format #t "~%")
  (format #t "  1) Linear Pipeline     - Traditional dev→qa→stg→prd~%")
  (format #t "  2) Parallel Validation - Simultaneous staging & UAT~%")
  (format #t "  3) Multi-Tenant        - Multiple staging environments~%")
  (format #t "  4) Ring Deployment     - Progressive user rollout~%")
  (format #t "  5) Blue-Green          - Instant switch deployment~%")
  (format #t "  6) Canary              - Gradual traffic shifting~%")
  (format #t "  7) Git Flow            - Branch-based deployment~%")
  (format #t "  8) GitHub Flow         - PR-based deployment~%")
  (format #t "  9) Pattern Comparison  - Metrics comparison table~%")
  (format #t "  0) Exit~%")
  (format #t "~%Enter choice (1-9, 0 to exit): "))

(define (run-interactive-demo)
  (display-menu)
  (let ((choice (read)))
    (case choice
      ((1) (simulate-release-pattern 'linear)
           (run-interactive-demo))
      ((2) (simulate-release-pattern 'parallel)
           (run-interactive-demo))
      ((3) (simulate-release-pattern 'multi-tenant)
           (run-interactive-demo))
      ((4) (simulate-release-pattern 'ring)
           (run-interactive-demo))
      ((5) (simulate-release-pattern 'blue-green)
           (run-interactive-demo))
      ((6) (simulate-release-pattern 'canary)
           (run-interactive-demo))
      ((7) (simulate-release-pattern 'git-flow)
           (run-interactive-demo))
      ((8) (simulate-release-pattern 'github-flow)
           (run-interactive-demo))
      ((9) (simulate-release-pattern 'comparison)
           (run-interactive-demo))
      ((0) (format #t "~%Goodbye!~%"))
      (else (format #t "~%Invalid choice. Please try again.~%")
            (run-interactive-demo)))))

;; Main execution
(format #t "~%Welcome to the Guile ChangeFlow Release Pattern Simulator!~%")
(format #t "This demo illustrates various deployment topologies and strategies.~%")

;; Check if running interactively
(if (isatty? (current-input-port))
    (run-interactive-demo)
    ;; Non-interactive mode: run all demos
    (begin
      (format #t "~%Running in non-interactive mode...~%")
      (simulate-release-pattern 'linear)
      (simulate-release-pattern 'parallel)
      (simulate-release-pattern 'ring)
      (simulate-release-pattern 'blue-green)
      (simulate-release-pattern 'canary)
      (simulate-release-pattern 'git-flow)
      (simulate-release-pattern 'github-flow)
      (simulate-release-pattern 'comparison)))