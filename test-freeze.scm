#!/usr/bin/env guile
!#

;;; Test Freeze Period Logic

(add-to-load-path "src")
(use-modules (risk freeze)
             (srfi srfi-19)
             (ice-9 format))

(display "Freeze Period Testing\n")
(display "=====================================\n\n")

;; Current status
(display "Current Status:\n")
(display (format #f "  In freeze period: ~a\n" (in-freeze-period?)))
(display (format #f "  Next window: ~a\n" (get-next-window)))
(display (format #f "  Freeze risk modifier: +~a points\n" (get-freeze-risk-modifier)))
(display (format #f "  Blackout dates: ~a\n" (check-blackout-dates)))

;; Show current time
(let* ((now (current-time))
       (date (time-utc->date now)))
  (display (format #f "\nCurrent time: ~a\n" date))
  (display (format #f "  Day of week: ~a (0=Sunday, 6=Saturday)\n" (date-week-day date)))
  (display (format #f "  Hour: ~a\n" (date-hour date)))
  (display (format #f "  Month: ~a\n" (date-month date)))
  (display (format #f "  Day: ~a\n" (date-day date))))

(display "\n=====================================\n")
(display "Freeze period rules:\n")
(display "  - Weekends (Saturday/Sunday): +30 risk points\n")
(display "  - After hours (before 9am or after 5pm): +20 risk points\n")
(display "  - December holidays (after Dec 15): +40 risk points\n")
(display "  - Black Friday week (Nov 20-30): +35 risk points\n")
(display "  - July 4th week (Jul 1-7): freeze period\n")