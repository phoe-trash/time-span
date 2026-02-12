(in-package :time-span)

(defclass duration ()
  ((days :accessor duration-days :initarg :days
         :initform 0 :type integer)
   (hours :accessor duration-hours :initarg :hours
          :initform 0 :type integer)
   (minutes :accessor duration-minutes :initarg :minutes
            :initform 0 :type integer)
   (seconds :accessor duration-seconds :initarg :seconds
            :initform 0 :type integer)
   (nanoseconds :accessor duration-nanoseconds :initarg :nanoseconds
                :initform 0 :type integer)
   (sign :accessor duration-sign :initarg :sign
         :initform 1 :type (member 1 0 -1)))
  (:documentation "A duration protocol class. Do not instantiate directly."))

(defclass year-month-duration (duration)
  ;; These slots are not allowed to be zero at the same time.
  ((years :accessor duration-years :initarg :years
          :initform 0 :type integer)
   (months :accessor duration-months :initarg :months
           :initform 0 :type integer))
  (:documentation
   "An ISO-8601-2 compatible duration containing month and year elements."))

(defclass week-duration (duration)
  ((weeks :accessor duration-weeks :initarg :weeks
          :initform 0 :type integer))
  (:documentation
   "An ISO-8601-2 compatible duration containing a week element."))
