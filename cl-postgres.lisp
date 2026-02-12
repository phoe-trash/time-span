(defpackage #:time-span.cl-postgres-reader
  (:use #:cl)
  (:local-nicknames (#:ts #:time-span))
  (:export #:set-time-span-cl-postgres-reader))

(in-package :time-span.cl-postgres-reader)

(defun set-time-span-cl-postgres-reader (&optional (table cl-postgres:*sql-readtable*))
  (cl-postgres::set-interval-reader
   (lambda (months days usec)
     (ts:duration :months months :days days :nanoseconds (* usec 1000)))
   table))

(defmethod cl-postgres:to-sql-string ((duration ts:duration))
  (format nil "'~A'::interval" (ts:format-iso8601-duration nil duration)))
