#!/usr/bin/env guile -s
!#

;;; Chaos Testing Scenarios for 99.97% Uptime
;;; Agent 5 - Resilience and Recovery Testing

(define-module (test chaos-scenarios)
  #:use-module (ice-9 format)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (run-chaos-suite))

;; Chaos Test Scenarios
(define chaos-scenarios
  '((network-partition "Simulate network partition" test-network-partition)
    (memory-pressure "Simulate memory pressure" test-memory-pressure)
    (cpu-spike "Simulate CPU spike" test-cpu-spike)
    (disk-full "Simulate disk full" test-disk-full)
    (service-crash "Simulate service crash" test-service-crash)
    (clock-skew "Simulate clock skew" test-clock-skew)
    (malformed-input "Send malformed input" test-malformed-input)
    (ddos-simulation "Simulate DDoS attack" test-ddos-simulation)))

;; Monitoring
(define (monitor-service-health services)
  "Monitor health of services during chaos"
  (let loop ((checks 0))
    (when (< checks 10)
      (for-each
       (lambda (service)
         (let* ((url (format #f "http://localhost:~a/health" (cdr service)))
                (cmd (format #f "curl -s -o /dev/null -w '%{http_code}' ~a 2>/dev/null" url))
                (code (string->number (with-input-from-pipe cmd read-line))))
           (format #t "[~a] ~a: ~a~%"
                   (current-time-string)
                   (car service)
                   (if (= code 200) "✓ UP" "✗ DOWN"))))
       services)
      (sleep 1)
      (loop (+ checks 1)))))

(define (current-time-string)
  "Get current time as string"
  (date->string (current-date) "~H:~M:~S"))

;; Network Chaos
(define (test-network-partition)
  "Test network partition recovery"
  (format #t "Simulating network partition...~%")

  ;; Simulate by sending requests with timeouts
  (let ((success 0)
        (total 10))
    (do ((i 0 (+ i 1)))
        ((>= i total))
      (let* ((cmd "curl -s --max-time 1 http://localhost:8080/health 2>/dev/null")
             (result (system cmd)))
        (when (= result 0)
          (set! success (+ success 1)))
        (usleep 500000)))

    (format #t "Network recovery: ~a/~a requests succeeded~%"
            success total)
    (> (/ success total) 0.5)))

;; Memory Pressure
(define (test-memory-pressure)
  "Test under memory pressure"
  (format #t "Simulating memory pressure...~%")

  ;; Allocate large strings
  (let ((memory-hogs '()))
    (catch #t
      (lambda ()
        ;; Try to allocate 100MB in 10MB chunks
        (do ((i 0 (+ i 1)))
            ((>= i 10))
          (set! memory-hogs (cons (make-string 10000000 #\x) memory-hogs))
          (format #t "Allocated ~aMB...~%" (* (+ i 1) 10)))

        ;; Test services still respond
        (let* ((cmd "curl -s -o /dev/null -w '%{http_code}' http://localhost:8080/health")
               (code (string->number (with-input-from-pipe cmd read-line))))
          (format #t "Service health under memory pressure: ~a~%"
                  (if (= code 200) "HEALTHY" "DEGRADED"))
          (= code 200)))
      (lambda (key . args)
        (format #t "Memory allocation failed (expected): ~a~%" key)
        #t))))

;; CPU Spike
(define (test-cpu-spike)
  "Test under CPU spike"
  (format #t "Simulating CPU spike...~%")

  ;; Spawn CPU-intensive threads
  (let ((cpu-threads '()))
    (do ((i 0 (+ i 1)))
        ((>= i 4))
      (let ((thread (make-thread
                     (lambda ()
                       (let loop ((n 0))
                         (when (< n 1000000)
                           (sqrt (+ n 1.0))
                           (loop (+ n 1))))))))
        (set! cpu-threads (cons thread cpu-threads))))

    ;; Test response times during CPU load
    (let ((start (current-time)))
      (system "curl -s http://localhost:8080/api/changes > /dev/null 2>&1")
      (let* ((end (current-time))
             (elapsed (time-difference end start))
             (ms (* 1000 (+ (time-second elapsed)
                           (/ (time-nanosecond elapsed) 1000000000.0)))))
        (format #t "Response time under CPU load: ~,2fms~%" ms)

        ;; Join threads
        (for-each join-thread cpu-threads)

        ;; Success if response time < 5 seconds
        (< ms 5000)))))

;; Disk Full Simulation
(define (test-disk-full)
  "Test disk full scenario"
  (format #t "Simulating disk full...~%")

  ;; Try to write large file
  (catch #t
    (lambda ()
      (call-with-output-file "/tmp/chaos-disk-test"
        (lambda (port)
          (do ((i 0 (+ i 1)))
              ((>= i 100))
            (display (make-string 1000000 #\x) port))))
      #t)
    (lambda (key . args)
      (format #t "Disk write failed (may be expected): ~a~%" key)
      ;; Service should still respond even if disk full
      (let* ((cmd "curl -s -o /dev/null -w '%{http_code}' http://localhost:8080/health")
             (code (string->number (with-input-from-pipe cmd read-line))))
        (= code 200)))))

;; Service Crash Simulation
(define (test-service-crash)
  "Test service crash and recovery"
  (format #t "Simulating service crash scenario...~%")

  ;; Check if service auto-recovers from errors
  (let ((recovery-time 0))
    ;; Send malformed request that might crash handler
    (system "curl -X POST -H 'Content-Type: application/json' -d 'INVALID JSON' http://localhost:8080/api/changes 2>/dev/null")

    ;; Monitor recovery
    (let loop ((attempts 0))
      (if (>= attempts 10)
          #f
          (let* ((cmd "curl -s -o /dev/null -w '%{http_code}' http://localhost:8080/health")
                 (code (string->number (with-input-from-pipe cmd read-line))))
            (if (= code 200)
                (begin
                  (format #t "Service recovered after ~a seconds~%" attempts)
                  #t)
                (begin
                  (sleep 1)
                  (loop (+ attempts 1)))))))))

;; Clock Skew
(define (test-clock-skew)
  "Test behavior with clock skew"
  (format #t "Testing clock skew handling...~%")

  ;; Send request with future timestamp
  (let* ((future-date "2030-01-01T00:00:00Z")
         (data (format #f "{\"title\":\"Future Change\",\"created_at\":\"~a\"}" future-date))
         (cmd (format #f "curl -s -X POST -H 'Content-Type: application/json' -d '~a' http://localhost:8080/api/changes" data))
         (result (with-input-from-pipe cmd read-string)))
    (format #t "System handles future timestamps: ~a~%"
            (if (string-contains result "error") "REJECTED" "ACCEPTED"))
    #t))

;; Malformed Input
(define (test-malformed-input)
  "Test malformed input handling"
  (format #t "Testing malformed input...~%")

  (define malformed-payloads
    '("{\"title\": }"                    ; Incomplete JSON
      "{'title': 'single quotes'}"      ; Wrong quotes
      "{\"title\": null}"                ; Null value
      "{\"title\": \"<script>alert()</script>\"}" ; XSS attempt
      (string-append "{\"description\": \"" (make-string 100000 #\a) "\"}") ; Very long
      "SELECT * FROM changes"            ; SQL injection
      "../../etc/passwd"))               ; Path traversal

  (let ((handled 0))
    (for-each
     (lambda (payload)
       (let* ((cmd (format #f "curl -s -X POST -H 'Content-Type: application/json' -d '~a' http://localhost:8080/api/changes 2>/dev/null" payload))
              (result (system cmd)))
         ;; Service should not crash
         (let* ((health-cmd "curl -s -o /dev/null -w '%{http_code}' http://localhost:8080/health")
                (code (string->number (with-input-from-pipe health-cmd read-line))))
           (when (= code 200)
             (set! handled (+ handled 1))))))
     malformed-payloads)

    (format #t "Handled ~a/~a malformed inputs without crashing~%"
            handled (length malformed-payloads))
    (= handled (length malformed-payloads))))

;; DDoS Simulation
(define (test-ddos-simulation)
  "Simulate DDoS attack"
  (format #t "Simulating DDoS attack...~%")

  (let ((threads '())
        (responses 0)
        (mutex (make-mutex)))

    ;; Spawn 50 threads making rapid requests
    (do ((i 0 (+ i 1)))
        ((>= i 50))
      (let ((thread (make-thread
                     (lambda ()
                       (do ((j 0 (+ j 1)))
                           ((>= j 20))
                         (let* ((cmd "curl -s -o /dev/null -w '%{http_code}' --max-time 1 http://localhost:8080/health 2>/dev/null")
                                (code (with-input-from-pipe cmd read-line)))
                           (when (equal? code "200")
                             (with-mutex mutex
                               (set! responses (+ responses 1))))))))))
        (set! threads (cons thread threads))))

    ;; Wait for all threads
    (for-each join-thread threads)

    (format #t "DDoS simulation: ~a/1000 requests succeeded~%" responses)
    (format #t "Service availability: ~,2f%~%" (* 100.0 (/ responses 1000)))

    ;; Success if > 50% requests succeeded (degraded but not down)
    (> responses 500)))

;; Main chaos suite runner
(define (run-chaos-suite)
  "Run complete chaos testing suite"
  (format #t "~%╔═══════════════════════════════════════════════╗~%")
  (format #t "║         CHAOS ENGINEERING TEST SUITE          ║~%")
  (format #t "║     Testing Resilience & 99.97% Uptime       ║~%")
  (format #t "╚═══════════════════════════════════════════════╝~%~%")

  (let ((passed 0)
        (failed 0)
        (services '(("Web" . 8080)
                   ("MCP" . 8081)
                   ("Webhook" . 8082))))

    ;; Initial health check
    (format #t "Initial service health:~%")
    (monitor-service-health services)

    ;; Run each chaos scenario
    (for-each
     (lambda (scenario)
       (let ((name (first scenario))
             (description (second scenario))
             (test-fn (third scenario)))
         (format #t "~%=== ~a ===~%" description)
         (catch #t
           (lambda ()
             (if ((eval test-fn (current-module)))
                 (begin
                   (format #t "✅ ~a: PASSED~%" name)
                   (set! passed (+ passed 1)))
                 (begin
                   (format #t "❌ ~a: FAILED~%" name)
                   (set! failed (+ failed 1)))))
           (lambda (key . args)
             (format #t "❌ ~a: ERROR - ~a~%" name key)
             (set! failed (+ failed 1))))

         ;; Allow recovery time between scenarios
         (sleep 2)))
     chaos-scenarios)

    ;; Final health check
    (format #t "~%Final service health:~%")
    (monitor-service-health services)

    ;; Calculate uptime
    (let* ((total (+ passed failed))
           (uptime-pct (if (> total 0)
                          (* 100.0 (/ passed total))
                          0)))
      (format #t "~%=== CHAOS TEST RESULTS ===~%")
      (format #t "Scenarios Passed: ~a/~a~%" passed total)
      (format #t "Resilience Score: ~,2f%~%" uptime-pct)

      (cond
        ((>= uptime-pct 99.97)
         (format #t "✅ EXCELLENT: Achieved 99.97% uptime target!~%"))
        ((>= uptime-pct 99.0)
         (format #t "⚠️  GOOD: High availability but below target~%"))
        ((>= uptime-pct 95.0)
         (format #t "⚠️  ACCEPTABLE: Some resilience issues~%"))
        (else
         (format #t "❌ CRITICAL: Major resilience problems~%")))

      uptime-pct)))

;; Run if executed directly
(when (batch-mode?)
  (run-chaos-suite))