(in-package :time-span-tests)

(5am:def-suite time-span-tests)
(5am:in-suite time-span-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Duration tests

(5am:test same-sign-or-zero-p
  (labels ((eqv (x y) (or (and x y) (and (not x) (not y))))
           (is (expected &rest numbers)
             (5am:is (eqv expected (apply #'time-span::same-sign-or-zero-p numbers))
                     "Failure: expected ~S for ~S" expected numbers)))
    (is t)
    (is t 0) (is t 0 0 0 0)
    (is t -1) (is t 1)
    (is t 1 0) (is t 0 1) (is t 1 1 1 1 1 1 1)
    (is t -1 0) (is t 0 -1) (is t -1 -1 -1 -1 -1 -1 -1)
    (is t 1 0 1 2 3 4 5) (is t -1 0 -1 -2 -3 -4 -5)
    (is nil -1 1) (is nil -1 0 1) (is nil 1 -1) (is nil 1 0 -1)
    (is nil -1 2 -3 4 -5 6)))

(5am:test denormalize-normalize
  (labels ((is (year month week day hour minute second nanosecond
                total-month total-nanosecond
                &optional (year2 year) (month2 month) (week2 week) (day2 day)
                  (hour2 hour) (minute2 minute) (second2 second) (nanosecond2 nanosecond))
             (let ((expected-1 (list total-month total-nanosecond))
                   (actual-1 (multiple-value-list
                              (ts:denormalize year month week day
                                              hour minute second nanosecond))))
               (5am:is (equal expected-1 actual-1)
                       "Failure in DENORMALIZE: expected ~S, got ~S"
                       expected-1 actual-1)
               (let ((expected-2 (list year2 month2 week2 day2
                                       hour2 minute2 second2 nanosecond2))
                     (actual-2 (multiple-value-list (apply #'ts:normalize actual-1))))
                 (5am:is (equal expected-2 actual-2)
                         "Failure in NORMALIZE: expected ~S, got ~S"
                         expected-2 actual-2)))))
    (is 0 0 0 0 0 0 0 0
        0 0)
    (is 1 0 0 0 0 0 0 0
        lt:+months-per-year+ 0)
    (is 0 1 0 0 0 0 0 0
        1 0)
    (is 0 0 1 0 0 0 0 0
        0 ts::+nanoseconds-per-week+)
    (is 0 0 0 1 0 0 0 0
        0 ts::+nanoseconds-per-day+)
    (is 0 0 0 0 1 0 0 0
        0 ts::+nanoseconds-per-hour+)
    (is 0 0 0 0 0 1 0 0
        0 ts::+nanoseconds-per-minute+)
    (is 0 0 0 0 0 0 1 0
        0 ts::+nanoseconds-per-second+)
    (is 0 0 0 0 0 0 0 1
        0 1)
    (is 1 2 3 4 5 6 7 8
        (+ lt:+months-per-year+ 2) (+ (* 3 ts::+nanoseconds-per-week+)
                                      (* 4 ts::+nanoseconds-per-day+)
                                      (* 5 ts::+nanoseconds-per-hour+)
                                      (* 6 ts::+nanoseconds-per-minute+)
                                      (* 7 ts::+nanoseconds-per-second+)
                                      8))
    (is 9 15 0 7 24 120 180 (+ (* 4 1000 1000 1000) 5)
        123 698584000000005
        10 3 1 1 2 3 4 5)))

(5am:test construct-duration-failure
  (macrolet ((fails (form) `(5am:signals ts:invalid-timestamp-element-signs ,form)))
    (fails (ts:duration :month 1 :nanosecond -1))
    (fails (ts:duration :month -1 :nanosecond 1))))

(5am:test duration-equality
  (5am:is (ts:duration= (ts:duration :day 1) (ts:duration :hour 24))))

(5am:test duration-comparison
  (5am:is (ts:duration< (ts:duration :day 1) (ts:duration :hour 48)))
  (5am:is (ts:duration> (ts:duration :day 1) (ts:duration :hour 12))))

(5am:test duration-in-units
  (5am:is (eql (ts:duration-as (ts:duration :day 1) :second) 86400)))

(5am:test duration-sum
  (5am:is (ts:duration= (ts:duration+ (ts:duration :day 1)
                                      (ts:duration :hour 12)
                                      (ts:duration :minute 30))
                        (ts:duration :second 131400))))

(5am:test duration-difference
  (5am:is (ts:duration= (ts:duration- (ts:duration :day 2)
                                      (ts:duration :day 1)
                                      (ts:duration :hour 12))
                        (ts:duration :second 43200))))

(5am:test duration-multiply
  (5am:is (ts:duration= (ts:duration* (ts:duration :day 1) 2)
                        (ts:duration :day 2))))

(5am:test duration-divide
  ;; TODO all other operators
  (5am:is (ts:duration= (ts:duration-truncate (ts:duration :day 2) 2)
                        (ts:duration :day 1))))

(5am:test duration-minimum
  (5am:is (ts:duration= (ts:duration-minimum (ts:duration :day 2)
                                             (ts:duration :hour 36)
                                             (ts:duration :day 4))
                        (ts:duration :hour 36))))

(defun gen-timestamp ()
  (lambda ()
    (flet ((rand-in-range (range-size)
             (- (random range-size) (/ range-size 2))))
      (lt:adjust-timestamp (lt:now)
        (:offset :year (rand-in-range 40))
        (:offset :month (rand-in-range 24))
        (:offset :day (rand-in-range 180))
        (:offset :minute (rand-in-range 600))
        (:offset :sec (rand-in-range 3600))
        (:offset :nsec (rand-in-range (expt 10 9)))))))

(5am:test duration-associates
  "Test that, for any pair of timestamps, this always holds:

  (+ b (difference a b)) == a"
  (let ((lt:*default-timezone* lt:+utc-zone+))
    (5am:for-all ((a (gen-timestamp))
                  (b (gen-timestamp)))
      (5am:is (lt:timestamp=
               a (ts:add-duration b (ts:timestamp-difference a b)))))))

(5am:test timestamp-difference
  (5am:is (ts:duration= (ts:timestamp-difference (lt:parse-timestring "2014-01-01T09:00:00")
                                                 (lt:parse-timestring "2014-01-01T04:30:00"))
                        (ts:duration :hour 4 :minute 30))))

(5am:test timestamp-add-duration
  (5am:is (lt:timestamp= (ts:add-duration (lt:parse-timestring "2014-01-01T09:00:00")
                                          (ts:duration :hour 3))
                         (lt:parse-timestring "2014-01-01T12:00:00"))))

(5am:test timestamp-subtract-duration
  (5am:is (lt:timestamp= (ts:subtract-duration (lt:parse-timestring "2014-01-01T09:00:00")
                                               (ts:duration :hour 3))
                         (lt:parse-timestring "2014-01-01T06:00:00"))))
