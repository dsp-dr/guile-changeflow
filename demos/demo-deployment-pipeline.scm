#!/usr/bin/env guile
!#

;;; demo-deployment-pipeline.scm - Interactive demo script
;;; Run with: guile -L src demo-deployment-pipeline.scm

(use-modules (simulator deployment-pipeline)
             (ice-9 format)
             (srfi srfi-1))

(define (print-banner)
  (format #t "~%")
  (format #t "┌────────────────────────────────────────────────────────────────────────────┐~%")
  (format #t "│  🎬 ITIL Deployment Pipeline Simulator - Live Demo                   │~%")
  (format #t "│                                                                        │~%")
  (format #t "│  🟢 Simulating 100 PRs across Frontend, Backend, IaC                  │~%")
  (format #t "│  🔄 Blocking staging deployments with 2-hour windows               │~%")
  (format #t "│  📋 ITIL change requests for each deployment                       │~%")
  (format #t "└────────────────────────────────────────────────────────────────────────────┘~%")
  (format #t "~%"))

(define (pause-for-effect seconds message)
  (format #t "~a" message)
  (force-output)
  (sleep seconds))

(define (main)
  (print-banner)
  
  (pause-for-effect 2 "🔧 Initializing deployment simulator...\n")
  
  ;; Run the 20-deployment demo
  (demo-20-deployments)
  
  (format #t "~%~%")
  (format #t "✨ Demo completed! This simulated:~%")
  (format #t "   • 100 mock PRs across 5 component types~%")
  (format #t "   • Realistic deployment times and failure rates~%")
  (format #t "   • Blocking staging environment (1 deployment at a time)~%")
  (format #t "   • ITIL change requests generated for each PR~%")
  (format #t "   • 2-hour deployment windows with rollback buffers~%~%")
  
  (format #t "🗺️ In a real system, this would connect to:~%")
  (format #t "   • GitHub API for actual PR data~%")
  (format #t "   • CI/CD systems (GitHub Actions, Jenkins)~%")
  (format #t "   • Infrastructure as Code (Terraform, CDK)~%")
  (format #t "   • Monitoring systems (Datadog, CloudWatch)~%")
  (format #t "   • Chat systems (Slack) for notifications~%~%")
  
  (format #t "🎆 This demonstrates the power of Guile + ITIL for\n")
  (format #t "    managing complex deployment workflows at scale!~%~%"))

;; Run the demo
(main)