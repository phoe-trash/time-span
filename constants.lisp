(in-package :time-span)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +months-per-year+ 12)
  (defconstant +nanoseconds-per-second+ (* 1000 1000 1000))
  (defconstant +nanoseconds-per-minute+
    (* +nanoseconds-per-second+ local-time:+seconds-per-minute+))
  (defconstant +nanoseconds-per-hour+
    (* +nanoseconds-per-second+ local-time:+seconds-per-hour+))
  (defconstant +nanoseconds-per-day+
    (* +nanoseconds-per-second+ local-time:+seconds-per-day+))
  (defconstant +nanoseconds-per-week+
    (* +nanoseconds-per-day+ local-time:+days-per-week+)))
