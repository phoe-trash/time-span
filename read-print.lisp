(in-package :time-span)

(defun duration-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((string (read stream t nil t)))
    (if (stringp string)
        (parse-iso8601-duration string)
        (error "Bad thing after #D: ~S" string))))

(defun install-duration-reader (&key (readtable *readtable*) force)
  (cond ((eq (get-dispatch-macro-character #\# #\D) #'duration-reader)
         (warn "Duration reader already installed."))
        ((or (null (get-dispatch-macro-character #\# #\D readtable)) force)
         (set-dispatch-macro-character #\# #\D #'duration-reader readtable))
        (t
         (warn "Unknown dispatch macro character for #D; not installing."))))

(defun uninstall-duration-reader (&key (readtable *readtable*) force)
  (if (or (eq (get-dispatch-macro-character #\# #\D readtable)
              #'duration-reader)
          force)
      (set-dispatch-macro-character #\# #\D nil readtable)
      (warn "Unknown dispatch macro character for #D; not uninstalling.")))

(defmethod print-object ((object duration) stream)
  (let ((string (format-iso8601-duration nil object)))
    (cond ((eq (get-dispatch-macro-character #\# #\D) #'duration-reader)
           (format stream "#D~S" string))
          (*print-readably*
           (let ((*print-readably* nil))
             (format stream "#.~S" `(format-iso8601-duration ,string))))
          (t
           (print-unreadable-object (object stream :type t)
             (format stream "~A" string))))))
