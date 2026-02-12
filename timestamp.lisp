(in-package :time-span)

(defun add-duration (timestamp duration)
  "Returns a fresh timestamp representing the time when `DURATION` has elapsed after `TIMESTAMP`."
  (local-time:adjust-timestamp timestamp
    (offset :day (day-of duration))
    (offset :sec (sec-of duration))
    (offset :nsec (nsec-of duration))))

(defun subtract-duration (timestamp duration)
  "Returns a fresh timestamp representing the time when `DURATION` will elapse before `TIMESTAMP`."
  (local-time:adjust-timestamp timestamp
    (offset :day (- (day-of duration)))
    (offset :sec (- (sec-of duration)))
    (offset :nsec (- (nsec-of duration)))))
