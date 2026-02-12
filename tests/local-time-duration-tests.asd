(defpackage :time-span-tests-system (:use #:asdf #:cl))
(in-package :time-span-tests-system)

(defsystem :time-span-tests
    :depends-on (:time-span :fiveam)
    :serial t
    :components
    ((:file "package")
     (:file "duration")
     (:file "timestamp")))
