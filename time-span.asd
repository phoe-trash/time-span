;; TODO handle cl-postgres
(defsystem #:time-span
  :description "Durations with ISO 8601-2 support and local-time integration"
  :version "1.0"
  :author "Michał \"phoe\" Herda and WebCheckout, Inc."
  :license "MIT"
  :depends-on (#:local-time #:alexandria #:esrap #:protest/base)
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
   (:file "timestamp"))
  :in-order-to ((test-op (test-op #:time-span/tests))))

(asdf:defsystem #:time-span/tests
  :depends-on (#:time-span #:fiveam)
  :serial t
  :pathname "tests"
  :components ((:file "package")
               (:file "tests"))
  :perform (test-op (o c)
                    (uiop:symbol-call
                     '#:5am '#:run!
                     (uiop:find-symbol*
                      '#:time-span-tests '#:time-span-tests))))
