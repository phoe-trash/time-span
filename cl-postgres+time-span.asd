(defsystem :cl-postgres+time-span
  :description "cl-postgres integration for time-span"
  :version "1.1"
  :author "Michał \"phoe\" Herda + WebCheckout, Inc."
  :license "MIT"
  :depends-on (:cl-postgres :time-span)
  :components ((:file "cl-postgres")))
