(in-package :time-span)

(e:defrule digit (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
  (:text t))

(e:defrule integer (+ digit)
  (:text t))

(e:defrule iso8601-quantity/whole
    integer
  (:function
    (lambda (string)
      (parse-integer string :radix 10))))

(e:defrule iso8601-quantity/fractional
    (and integer (or #\, #\.) integer)
  (:destructure (whole _ fraction)
    (declare (ignore _))
    (+ (parse-integer whole :radix 10)
       (/ (parse-integer fraction :radix 10)
          (expt 10 (length fraction))))))

(e:defrule iso8601-year (and iso8601-quantity/whole #\Y)
  (:lambda (production) (list :year (first production))))

(e:defrule iso8601-month (and iso8601-quantity/whole #\M)
  (:lambda (production) (list :month (first production))))

(e:defrule iso8601-week (and iso8601-quantity/whole #\W)
  (:lambda (production) (list :week (first production))))

(e:defrule iso8601-day (and iso8601-quantity/whole #\D)
  (:lambda (production) (list :day (first production))))

(e:defrule iso8601-hour (and iso8601-quantity/whole #\H)
  (:lambda (production) (list :hour (first production))))

(e:defrule iso8601-minute (and iso8601-quantity/whole #\M)
  (:lambda (production) (list :minute (first production))))

(e:defrule iso8601-second (and iso8601-quantity/fractional #\S)
  (:lambda (production) (list :second (first production))))

(e:defrule iso8601-duration-date
    (and (e:? iso8601-year)
         (e:? iso8601-month)
         (e:? iso8601-week)
         (e:? iso8601-day))
  (:lambda (production)
    (apply #'append production)))

(e:defrule iso8601-duration-time
    (and #\T
         (e:? iso8601-hour)
         (e:? iso8601-minute)
         (e:? iso8601-second))
  (:lambda (production)
    (apply #'append (cdr production))))

(defun make-duration-from-time-values (values)
  (let ((years 0)
        (months 0)
        (weeks 0)
        (days 0)
        (hours 0)
        (minutes 0)
        (seconds 0))
    (loop for (key value) on values by #'cddr
          do (ecase key
               (:year (incf years value))
               (:month (incf months value))
               (:week (incf weeks value))
               (:day (incf days value))
               (:hour (incf hours value))
               (:minute (incf minutes value))
               (:second (incf seconds value))))
    (multiple-value-bind (seconds nanoseconds)
        (truncate (* seconds +nanoseconds-per-second+)
                  +nanoseconds-per-second+)
      (duration :years years
                :months months
                :weeks weeks
                :days days
                :hours hours
                :minutes minutes
                :seconds seconds
                ;; We may lose precision here.
                ;; Hopefully no one wants picoseconds.
                :nanoseconds (truncate nanoseconds)))))

(e:defrule iso8601-date-T-time
    (and iso8601-duration-date (e:? iso8601-duration-time))
  (:destructure (date time)
    (make-duration-from-time-values (append date time))))

(defun char-string-to-integer (chars)
  (parse-integer (apply #'concatenate 'string chars) :radix 10))

(defun production-ymd-to-list (year month day)
  (list :year (char-string-to-integer year)
        :month (char-string-to-integer month)
        :day (char-string-to-integer day)))

(defun production-hms-to-list (hour minute second)
  (list :hour (char-string-to-integer hour)
        :minute (char-string-to-integer minute)
        :second (char-string-to-integer second)))

(e:defrule iso8601-date-full
    (and (and digit digit digit digit)
         #\- (and digit digit)
         #\- (and digit digit))
  (:lambda (production)
    (production-ymd-to-list
     (first production) (third production) (fifth production))))

(e:defrule iso8601-time-full
    (and (and digit digit) #\: (and digit digit) #\: (and digit digit))
  (:lambda (production)
    (production-hms-to-list (first production)
                            (third production)
                            (fifth production))))

(e:defrule iso8601-date-time-full
    (and iso8601-date-full #\T iso8601-time-full)
  (:destructure (date _T time)
    (declare (ignore _T))
    (make-duration-from-time-values (append date time))))

(e:defrule iso8601-date-compact
    (and (and digit digit digit digit) (and digit digit) (and digit digit))
  (:destructure (year month day)
    (production-ymd-to-list year month day)))

(e:defrule iso8601-time-compact
    (and (and digit digit) (and digit digit) (and digit digit))
  (:destructure (hour minute second)
    (production-hms-to-list hour minute second)))

(e:defrule iso8601-date-time-compact
    (and iso8601-date-compact #\T iso8601-time-compact)
  (:destructure (date _T time)
    (declare (ignore _T))
    (make-duration-from-time-values (append date time))))

(e:defrule iso8601-duration
    (and #\P
         (or iso8601-date-time-full
             iso8601-date-time-compact
             iso8601-date-T-time))
  (:destructure (P duration)
    (declare (ignore P))
    duration))

(defun parse-iso8601-duration (string)
  "Parser for ISO8601 durations (with limitations) returning DURATION instances.
http://en.wikipedia.org/wiki/ISO_8601#Durations"
  (esrap:parse 'iso8601-duration string))

(defun format-iso8601-duration (destination duration)
  (multiple-value-call #'format destination
    ;; I'm not even sorry.
    "P~[~[~[~[~[~[~[~[0S~^~:;~8:*~]~:;~7:*~]~:;~6:*~
      ~]~:;~5:*~]~:;~4:*~]~:;~3:*~]~:;~2:*~]~:;~:*~]~
      ~[~:;~:*~DY~]~[~:;~:*~DM~]~[~:;~:*~DW~]~[~:;~:*~DD~]~
      ~[~[~[~[~^~:;~4:*~]~:;~3:*~]~:;~2:*~]~:;~:*~]T~
      ~[~:;~:*~DH~]~[~:;~:*~DM~]~
      ~[~[~:;~:*0.~DS~]~:;~[~2:*~DS~:;~2:*~D.~DS~]~]"
    ;; ...okay, that wasn't fair. Here, a brief rundown.
    ;; The first two lines handle the all-zeros case, printing "P0S"
    ;; or restoring already checked zeroes onto the argument list.
    ;; The third line outputs years, months, weeks, days.
    ;; The fourth line terminates formatting if the time part is zero.
    ;; The fifth line outputs hours and minutes.
    ;; The sixth line has conditional printing for seconds and nanoseconds.
    (decode-duration duration)))
