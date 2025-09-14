(define-module (risk freeze)
  #:use-module (srfi srfi-19)
  #:export (in-freeze-period?
            get-next-window
            check-blackout-dates
            get-freeze-risk-modifier))

(define (in-freeze-period?)
  "Check if we're in a deployment freeze period"
  (let* ((now (current-time))
         (date (time-utc->date now))
         (hour (date-hour date))
         (day (date-week-day date))
         (month (date-month date))
         (day-of-month (date-day date)))

    (or
     ;; Weekends (Sunday = 0, Saturday = 6)
     (or (= day 0) (= day 6))

     ;; After hours (before 9am or after 5pm)
     (or (< hour 9) (> hour 17))

     ;; December freeze (holidays - last two weeks)
     (and (= month 12) (> day-of-month 15))

     ;; Black Friday week (US Thanksgiving week)
     (and (= month 11) (>= day-of-month 20) (<= day-of-month 30))

     ;; July 4th week (US Independence Day)
     (and (= month 7) (>= day-of-month 1) (<= day-of-month 7)))))

(define (get-next-window)
  "Get next deployment window"
  (if (in-freeze-period?)
      "Next Monday 9:00 AM (business hours)"
      "Now - deployment window open"))

(define (check-blackout-dates)
  "Check for specific blackout dates - returns list of upcoming blackouts"
  (let* ((now (current-time))
         (date (time-utc->date now))
         (month (date-month date))
         (day-of-month (date-day date)))
    (cond
      ((and (= month 11) (< day-of-month 20))
       '("Black Friday week approaching (Nov 20-30)"))
      ((and (= month 12) (< day-of-month 15))
       '("Holiday freeze approaching (Dec 15-31)"))
      ((and (= month 6) (> day-of-month 15))
       '("July 4th freeze approaching (Jul 1-7)"))
      (else '()))))

(define (get-freeze-risk-modifier)
  "Get additional risk points for being in freeze period"
  (let* ((now (current-time))
         (date (time-utc->date now))
         (hour (date-hour date))
         (day (date-week-day date))
         (month (date-month date))
         (day-of-month (date-day date)))
    (cond
      ;; Critical freeze periods
      ((and (= month 12) (> day-of-month 15)) 40)  ; Holiday freeze
      ((and (= month 11) (>= day-of-month 20) (<= day-of-month 30)) 35)  ; Black Friday

      ;; Weekend deployments
      ((or (= day 0) (= day 6)) 30)

      ;; After hours deployments
      ((or (< hour 9) (> hour 17)) 20)

      ;; Regular business hours
      (else 0))))