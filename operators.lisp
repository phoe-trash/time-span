(in-package :time-span)

(defun duration+ (&rest durations)
  "Returns a fresh duration representing the sum of the lengths of its
arguments."
  (let ((total-months 0)
        (total-nanoseconds 0))
    (dolist (duration durations)
      (multiple-value-bind (months nanoseconds)
          (apply #'denormalize (decode-duration duration))
        (incf total-months months)
        (incf total-nanoseconds nanoseconds)))
    (check-sign (signum total-months) (signum total-nanoseconds))
    (duration :months total-months :nanoseconds total-nanoseconds)))

(defun duration- (duration &rest durations)
  "Returns a fresh duration representing the result of subtracting the length of
each argument in turn."
  (multiple-value-bind (total-months total-nanoseconds)
      (apply #'denormalize (decode-duration duration))
    (dolist (duration durations)
      (multiple-value-bind (months nanoseconds)
          (apply #'denormalize (decode-duration duration))
        (decf total-months months)
        (decf total-nanoseconds nanoseconds)))
    (check-sign (signum total-months) (signum total-nanoseconds))
    (duration :months total-months :nanoseconds total-nanoseconds)))

(defun duration* (duration factor)
  "Returns a fresh duration as long as `DURATION` multiplied by `FACTOR`."
  (multiple-value-bind (months nanoseconds)
      (apply #'denormalize (decode-duration duration))
    (duration :months (* months factor) :nanoseconds (* nanoseconds factor))))

(macrolet
    ((define-variant (symbol)
       (let ((name (alexandria:symbolicate '#:duration- symbol)))
         `(defun ,name (duration divisor)
            ,(format nil "Returns a fresh duration that is as long as ~
                          `DURATION` divided by `DIVISOR`~%via `~A`." symbol)
            (multiple-value-bind (months nanoseconds)
                (apply #'denormalize (decode-duration duration))
              (duration :months (,symbol months divisor)
                        :nanoseconds (,symbol nanoseconds divisor)))))))
  (define-variant floor)
  (define-variant ceiling)
  (define-variant truncate)
  (define-variant round))

(defun duration-minimum (duration &rest durations)
  (alexandria:extremum (cons duration durations) #'duration<))

(defun duration-maximum (duration &rest durations)
  (alexandria:extremum (cons duration durations) #'duration>))
