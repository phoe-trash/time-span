;; TODO incompatibly modify system
;; TODO handle cl-postgres
(defsystem :time-span
  :description "Durations with ISO8601 support and local-time integration"
  :version "1.0"
  :author "Michał \"phoe\" Herda and WebCheckout, Inc."
  :license "MIT"
  :depends-on (:local-time :alexandria :esrap)
  :serial t
  :components
  ((:file "package")
   (:file "constants")
   (:file "class-definition")
   (:file "constructors")
   (:file "comparators")
   (:file "operators")
   (:file "iso8601")
   (:file "read-print")
   (:file "timestamp")))
