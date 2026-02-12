(in-package :cl-user)

;; TODO define-package indent function
(defpackage #:time-span
  (:use #:common-lisp)
  (:local-nicknames (#:e #:esrap))
  ;; class-definition.lisp
  (:export
   #:duration
   #:duration-days
   #:duration-hours
   #:duration-minutes
   #:duration-seconds
   #:duration-nanoseconds
   #:duration-sign
   #:year-month-duration
   #:duration-years
   #:week-duration
   #:duration-months
   #:duration-weeks)
  ;; constructors.lisp
  (:export
   #:denormalize
   #:normalize
   #:duration
   #:decode-duration
   #:duration-as)
  ;; comparators.lisp
  (:export
   #:duration=
   #:duration/=
   #:duration>
   #:duration>=
   #:duration<
   #:duration<=)
  ;; operators.lisp
  (:export
   #:duration+
   #:duration-
   #:duration*
   #:duration-floor
   #:duration-ceiling
   #:duration-truncate
   #:duration-round
   #:duration-minimum
   #:duration-maximum)
  ;; iso8601.lisp
  (:export
   #:parse-iso8601-duration
   #:format-iso8601-duration)
  ;; read-print.lisp
  (:export
   #:duration-reader
   #:install-duration-reader
   #:uninstall-duration-reader)
  ;; timestamp.lisp
  (:export
   #:add-duration
   #:subtract-duration))
