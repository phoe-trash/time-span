(in-package :time-span)

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

(define-condition invalid-timestamp-element-signs (arithmetic-error) ()
  (:report (lambda (condition stream)
             (format stream
                     "~@<Unable to create a year-month timestamp with divergent ~
                         component signs for (~A ~{~A~^ ~})~:@>"
                     (arithmetic-error-operation condition)
                     (arithmetic-error-operands condition)))))

(declaim (inline check-sign))
(defun check-sign (msign nsign function arguments)
  (unless (same-sign-or-zero-p msign nsign)
    (error 'invalid-timestamp-element-signs :operation function :operands arguments)))

(defun denormalize (year month week day hour minute second nanosecond)
  "Converts the provided duration components into total numbers of months and
nanoseconds."
  (let* ((total-month (+ month (* +months-per-year+ year)))
         ;; We do not support leap seconds of any kind here.
         (total-nanosecond (+ nanosecond
                              (* +nanoseconds-per-second+ second)
                              (* +nanoseconds-per-minute+ minute)
                              (* +nanoseconds-per-hour+ hour)
                              (* +nanoseconds-per-day+ day)
                              (* +nanoseconds-per-week+ week)))
         (msign (signum total-month))
         (nsign (signum total-nanosecond)))
    (check-sign msign nsign 'denormalize (list year month week day hour
                                               minute second nanosecond))
    (values total-month total-nanosecond)))

(defun normalize (total-month total-nanosecond)
  "Converts the provided total numbers of months and nanoseconds into
duration components."
  (let ((msign (signum total-month))
        (nsign (signum total-nanosecond)))
    (check-sign msign nsign 'normalize (list total-month total-nanosecond))
    (multiple-value-bind (year month)
        (floor total-month +months-per-year+)
      (multiple-value-bind (week remaining-nanosecond)
          (floor total-nanosecond +nanoseconds-per-week+)
        (multiple-value-bind (day remaining-nanosecond)
            (floor remaining-nanosecond +nanoseconds-per-day+)
          (multiple-value-bind (hour remaining-nanosecond)
              (floor remaining-nanosecond +nanoseconds-per-hour+)
            (multiple-value-bind (minute remaining-nanosecond)
                (floor remaining-nanosecond +nanoseconds-per-minute+)
              (multiple-value-bind (second nanosecond)
                  (floor remaining-nanosecond +nanoseconds-per-second+)
                (values year month week day
                        hour minute second nanosecond)))))))))

(defun duration (&key (year 0) (month 0) (week 0) (day 0) (hour 0)
                   (minute 0) (second 0) (nanosecond 0))
  "Returns a new duration instance representing the sum of the `DAY`, `HOUR`,
`MINUTE`, `SECOND`, and `NANOSECOND` arguments and either `WEEK` or `YEAR`
and `MONTH`.
Durations are normalized, that is, (duration :hour 1) and (duration :minute
60) will result in duration instances with the same internal representation.
Year and month elements are an exception, since they cannot be normalized
losslessly."
  (flet ((nonzerop (x) (not (zerop x))))
    ;; TODO maybe this whole dance is not necessary and we can have weeks
    ;; along with months/years. Check with ISO-8601-2.
    ;; TODO checked, yup, we can have them. Time to simplify the class
    ;; structure again and remove unneeded PROTEST/BASE dependency.
    (unless (and (zerop month) (zerop year))
      (setf days (+ days (* 7 weeks))
            weeks 0)))
  (multiple-value-bind (total-month total-nanosecond)
      (denormalize year month week day hour minute second nanosecond)
    (multiple-value-bind (year month week day hour minute second nanosecond)
        (normalize (abs total-month) (abs total-nanosecond))
      (let ((sign (if (zerop (signum total-month))
                      (signum total-nanosecond)
                      (signum total-month))))
        ;; Prefer to create week durations, since they're unambiguous.
        (cond ((and (zerop month) (zerop year))
               (make-instance 'week-duration
                              :week week
                              :day day
                              :hour hour
                              :minute minute
                              :second second
                              :nanosecond nanosecond
                              :sign sign))
              ((zerop week)
               (make-instance 'year-month-duration
                              :year year
                              :month month
                              :day day
                              :hour hour
                              :minute minute
                              :second second
                              :nanosecond nanosecond
                              :sign sign))
              (t (error "Internal error in DURATION.")))))))

(defgeneric decode-duration (duration)
  (:documentation
   "Returns, as multiple values, DURATION's logical components:

(year month week day hour minute seconds nanosecond)

Depending on the duration type, either WEEK or MONTH and YEAR will
be 0.

The sign of the duration is encoded into each component.")
  (:method ((duration year-month-duration))
    (let ((sign (duration-sign duration)))
      (values (* sign (duration-year duration))
              (* sign (duration-month duration))
              0
              (* sign (duration-day duration))
              (* sign (duration-hour duration))
              (* sign (duration-minute duration))
              (* sign (duration-second duration))
              (* sign (duration-nanosecond duration)))))
  (:method ((duration week-duration))
    (let ((sign (duration-sign duration)))
      (values 0
              0
              (* sign (duration-week duration))
              (* sign (duration-day duration))
              (* sign (duration-hour duration))
              (* sign (duration-minute duration))
              (* sign (duration-second duration))
              (* sign (duration-nanosecond duration))))))

(deftype duration-as-unit ()
  '(member :year :month :week :day :hour :minute :second :nanosecond))

(defun duration-as (duration unit)
  "Returns two values: the first is the number of whole `UNIT`s within
`DURATION`, and the second is a fresh duration representing the reamainder of
the original duration after dividing it by `UNIT`. May return `NIL` as the first
value if the conversion does not make sense. `UNIT` must be one of :YEAR,
:MONTH, :WEEK, :DAY, :HOUR, :MINUTE, :SECOND, and :NANOSECOND."
  (declare (type duration duration)
           (type duration-as-unit unit))
  ;; We can only represent a year-month duration as years or months.
  (when (and (typep duration 'year-month-duration)
             (not (member unit '(:year :month))))
    (return-from duration-as (values nil duration)))
  ;; We cannot represent a week duration as years or months.
  (when (or (and (typep duration 'week-duration)
                 (member unit '(:year :month))))
    (return-from duration-as (values nil duration)))
  ;; The representation makes sense; proceed.
  (multiple-value-bind (total-month total-nanosecond)
      (apply #'denormalize (multiple-value-list (decode-duration duration)))
    (ecase unit
      ;; Year-month duration.
      (:year (multiple-value-bind (year month)
                 (truncate total-month +months-per-year+)
               (values year (duration :month month
                                      :nanosecond total-nanosecond))))
      (:month (values total-month (duration :nanosecond total-nanosecond)))
      ;; Week duration.
      (:week (multiple-value-bind (week nanosecond)
                 (truncate total-nanosecond +nanoseconds-per-week+)
               (values week (duration :nanosecond nanosecond))))
      (:day (multiple-value-bind (day nanosecond)
                (truncate total-nanosecond +nanoseconds-per-day+)
              (values day (duration :nanosecond nanosecond))))
      (:hour (multiple-value-bind (hour nanosecond)
                 (truncate total-nanosecond +nanoseconds-per-hour+)
               (values hour (duration :nanosecond nanosecond))))
      (:minute (multiple-value-bind (minute nanosecond)
                   (truncate total-nanosecond +nanoseconds-per-minute+)
                 (values minute (duration :nanosecond nanosecond))))
      (:second (multiple-value-bind (second nanosecond)
                   (truncate total-nanosecond +nanoseconds-per-second+)
                 (values second (duration :nanosecond nanosecond))))
      (:nanosecond (values total-nanosecond (duration))))))
