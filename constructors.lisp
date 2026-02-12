(in-package :ltd)

(defun same-sign-or-zero-p (&rest numbers)
  (declare (dynamic-extent numbers))
  (cond ((null numbers) t)
        ((null (cdr numbers)) t)
        (t
         (flet ((fail () (return-from same-sign-or-zero-p nil)))
           (let ((current-sign 0))
             (dolist (number numbers t)
               (case current-sign
                 (1 (case (signum number)
                      (1)
                      (0)
                      (-1 (fail))))
                 (0 (case (signum number)
                      (1 (setf current-sign 1))
                      (0)
                      (-1 (setf current-sign -1))))
                 (-1 (case (signum number)
                       (1 (fail))
                       (0)
                       (-1))))))))))

(declaim (inline check-sign))
(defun check-sign (msign nsign)
  (unless (same-sign-or-zero-p msign nsign)
    ;; TODO arithmetic-error?
    (error "~@<Unable to create a year-month timestamp with divergent ~
               component signs.~:@>")))

(defun denormalize (years months weeks days
                    hours minutes seconds nanoseconds)
  "Converts the provided duration components into total numbers of months and
nanoseconds."
  (let* ((total-months (+ months
                          (* +months-per-year+ years)))
         ;; We do not support leap seconds of any sort.
         (total-nanoseconds (+ nanoseconds
                               (* +nanoseconds-per-second+ seconds)
                               (* +nanoseconds-per-minute+ minutes)
                               (* +nanoseconds-per-hour+ hours)
                               (* +nanoseconds-per-day+ days)
                               (* +nanoseconds-per-week+ weeks)))
         (msign (signum total-months))
         (nsign (signum total-nanoseconds)))
    (check-sign msign nsign)
    (values total-months total-nanoseconds)))

(defun normalize (total-months total-nanoseconds)
  "Converts the provided total numbers of months and nanoseconds into
duration components."
  (let ((msign (signum total-months))
        (nsign (signum total-nanoseconds)))
    (check-sign msign nsign)
    (multiple-value-bind (years months)
        (floor total-months +months-per-year+)
      (multiple-value-bind (weeks remaining-nanoseconds)
          (floor total-nanoseconds +nanoseconds-per-week+)
        (multiple-value-bind (days remaining-nanoseconds)
            (floor remaining-nanoseconds +nanoseconds-per-day+)
          (multiple-value-bind (hours remaining-nanoseconds)
              (floor remaining-nanoseconds +nanoseconds-per-hour+)
            (multiple-value-bind (minutes remaining-nanoseconds)
                (floor remaining-nanoseconds +nanoseconds-per-minute+)
              (multiple-value-bind (seconds nanoseconds)
                  (floor remaining-nanoseconds +nanoseconds-per-second+)
                ;; Return all values as-is. It's up to the client to make sense
                ;; of them if years/months and weeks are present together.
                (values years months weeks days
                        hours minutes seconds nanoseconds)))))))))

(defun duration (&key (years 0) (months 0) (weeks 0) (days 0) (hours 0)
                   (minutes 0) (seconds 0) (nanoseconds 0))
  "Returns a new duration instance representing the sum of the `DAYS`, `HOURS`,
`MINUTES`, `SECONDS`, and `NANOSECONDS` arguments and either `WEEKS` or `YEARS`
and `MONTHS`.
Durations are normalized, that is, (duration :hours 1) and (duration :minutes
60) will result in duration instances with the same internal representation.
Year and month elements are an exception, since they cannot be normalized
losslessly."
  (flet ((nonzerop (x) (not (zerop x))))
    (when (and (nonzerop weeks) (or (nonzerop years) (nonzerop months)))
      (error "~@<Invalid timestamp - weeks are not allowed to be passed ~
                 together with years or months.~:@>")))
  (multiple-value-bind (total-months total-nanoseconds)
      (denormalize years months weeks days hours minutes seconds nanoseconds)
    (multiple-value-bind (years months weeks days
                          hours minutes seconds nanoseconds)
        (normalize (abs total-months) (abs total-nanoseconds))
      (let ((sign (if (zerop (signum total-months))
                      (signum total-nanoseconds)
                      (signum total-months))))
        ;; Prefer to create week durations, since they're unambiguous.
        (if (and (zerop months) (zerop years))
            (make-instance 'week-duration
                           :weeks weeks
                           :days days
                           :hours hours
                           :minutes minutes
                           :seconds seconds
                           :nanoseconds nanoseconds
                           :sign sign)
            (make-instance 'year-month-duration
                           :years years
                           :months months
                           :days (+ (* 7 weeks)
                                    days)
                           :hours hours
                           :minutes minutes
                           :seconds seconds
                           :nanoseconds nanoseconds
                           :sign sign))))))

(defgeneric decode-duration (duration)
  (:documentation
   "Returns, as multiple values, DURATION's logical components:

(years months weeks days hours minutes seconds nanoseconds)

Depending on the duration type, either WEEKS or MONTHS and YEARS
will be 0.

The sign of the duration is encoded into each component.")
  (:method ((duration year-month-duration))
    (let ((sign (duration-sign duration)))
      (values (* sign (duration-years duration))
              (* sign (duration-months duration))
              0
              (* sign (duration-days duration))
              (* sign (duration-hours duration))
              (* sign (duration-minutes duration))
              (* sign (duration-seconds duration))
              (* sign (duration-nanoseconds duration)))))
  (:method ((duration week-duration))
    (let ((sign (duration-sign duration)))
      (values 0
              0
              (* sign (duration-weeks duration))
              (* sign (duration-days duration))
              (* sign (duration-hours duration))
              (* sign (duration-minutes duration))
              (* sign (duration-seconds duration))
              (* sign (duration-nanoseconds duration))))))

(deftype duration-as-unit ()
  '(member :years :months :weeks :days :hours :minutes :seconds :nanoseconds))

(defun duration-as (duration unit)
  "Returns two values: the first is the number of whole `UNIT`s within
`DURATION`, and the second is a fresh duration representing the reamainder of
the original duration after dividing it by `UNIT`. May return `NIL` as the first
value if the conversion does not make sense. `UNIT` must be one of :YEARS,
:MONTHS, :WEEKS, :DAYS, :HOURS, :MINUTES, :SECONDS, and :NANOSECONDS."
  (declare (type duration duration)
           (type duration-as-unit unit))
  ;; We can only represent a year-month duration as years or months.
  (when (and (typep duration 'year-month-duration)
             (not (member unit '(:years :months))))
    (return-from duration-as (values nil duration)))
  ;; We cannot represent a week duration as years or months.
  (when (or (and (typep duration 'week-duration)
                 (member unit '(:years :months))))
    (return-from duration-as (values nil duration)))
  (multiple-value-bind (total-months total-nanoseconds)
      (apply #'denormalize (decode-duration duration))
    (ecase unit
      ;; Year-month duration.
      (:years (multiple-value-bind (years months)
                  (truncate total-months +months-per-year+)
                (values years (duration :months months
                                        :nanoseconds total-nanoseconds))))
      (:months (values total-months (duration :nanoseconds total-nanoseconds)))
      ;; Week duration.
      (:weeks (multiple-value-bind (weeks nanoseconds)
                  (truncate total-nanoseconds +nanoseconds-per-week+)
                (values weeks (duration :nanoseconds nanoseconds))))
      (:days (multiple-value-bind (days nanoseconds)
                 (truncate total-nanoseconds +nanoseconds-per-day+)
               (values days (duration :nanoseconds nanoseconds))))
      (:hours (multiple-value-bind (hours nanoseconds)
                  (truncate total-nanoseconds +nanoseconds-per-hour+)
                (values hours (duration :nanoseconds nanoseconds))))
      (:minutes (multiple-value-bind (minutes nanoseconds)
                    (truncate total-nanoseconds +nanoseconds-per-minute+)
                  (values minutes (duration :nanoseconds nanoseconds))))
      (:seconds (multiple-value-bind (seconds nanoseconds)
                    (truncate total-nanoseconds +nanoseconds-per-second+)
                  (values seconds (duration :nanoseconds nanoseconds))))
      (:nanoseconds (values total-nanoseconds (duration))))))
