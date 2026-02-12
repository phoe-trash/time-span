(in-package :time-span)

(defun %year-month-duration-compare (a b)
  (declare (type year-month-duration a b))
  (flet ((%year-month-duration= (a b)
           ;; Sign is checked in %DURATION-COMPARE.
           (and (= (duration-years a) (duration-years b))
                (= (duration-months a) (duration-months b))
                (= (duration-days a) (duration-days b))
                (= (duration-hours a) (duration-hours b))
                (= (duration-minutes a) (duration-minutes b))
                (= (duration-seconds a) (duration-seconds b))
                (= (duration-nanoseconds a) (duration-nanoseconds b))
                '=)))
    (declare (inline %year-month-duration=))
    (cond
      ;; If any components other than year and month are filled, it's impossible
      ;; to compare, as "complete" year-month durations have no linear order.
      ;; Examples: P1Y and P365D, P1Y and P366D.
      ;; Order aside, we can still test for equality though.
      ((and (or (/= 0 (duration-years a)) (/= 0 (duration-months a)))
            (or (/= 0 (duration-days a)) (/= 0 (duration-hours a))
                (/= 0 (duration-minutes a)) (/= 0 (duration-seconds a))
                (/= 0 (duration-nanoseconds a))))
       (%year-month-duration= a b))
      ((and (or (/= 0 (duration-years b)) (/= 0 (duration-months b)))
            (or (/= 0 (duration-days b)) (/= 0 (duration-hours b))
                (/= 0 (duration-minutes b)) (/= 0 (duration-seconds b))
                (/= 0 (duration-nanoseconds b))))
       (%year-month-duration= a b))
      ;; Only year and month components are filled. We can meaningfully compare.
      ((< (duration-years a) (duration-years b)) '<)
      ((> (duration-years a) (duration-years b)) '>)
      ((< (duration-months a) (duration-months b)) '<)
      ((> (duration-months a) (duration-months b)) '>)
      ;; Years and months are the same, all other slots are zero.
      ;; Must be equal.
      (t '=))))

(defun %week-duration-compare (a b)
  (declare (type week-duration a b))
  (cond
    ((< (duration-weeks a) (duration-weeks b)) '<)
    ((> (duration-weeks a) (duration-weeks b)) '>)
    ((< (duration-days a) (duration-days b)) '<)
    ((> (duration-days a) (duration-days b)) '>)
    ((< (duration-hours a) (duration-hours b)) '<)
    ((> (duration-hours a) (duration-hours b)) '>)
    ((< (duration-minutes a) (duration-minutes b)) '<)
    ((> (duration-minutes a) (duration-minutes b)) '>)
    ((< (duration-seconds a) (duration-seconds b)) '<)
    ((> (duration-seconds a) (duration-seconds b)) '>)
    ((< (duration-nanoseconds a) (duration-nanoseconds b)) '<)
    ((> (duration-nanoseconds a) (duration-nanoseconds b)) '>)
    (t '=)))

(defun %duration-compare (a b)
  ;; May return <, >, =, or NIL if two durations are not comparable.
  (declare (type duration a b))
  (let ((a-week-p (typep a 'week-duration))
        (b-week-p (typep b 'week-duration)))
    (cond
      ;; Are they of different signs?
      ((< (duration-sign a) (duration-sign b)) '<)
      ((> (duration-sign a) (duration-sign b)) '>)
      ;; If they are of different types, comparison is meaningless.
      ((alexandria:xor a-week-p b-week-p) nil)
      ;; Same type. Are they year-month durations?
      (a-week-p (%year-month-duration-compare a b))
      ;; Then they must be week durations.
      (t (%week-duration-compare a b)))))

;;; FIXME -- this is just copied from local-time and modified to handle different types
;;;          with an additional PARSE-BODY

(defmacro %defcomparator (name (type) &body body)
  (multiple-value-bind (body decls docstring) (alexandria:parse-body body :documentation t)
    (let ((pair-comparator-name (intern (concatenate 'string "%" (string name)))))
      `(progn
         (declaim (inline ,pair-comparator-name))
         (defun ,pair-comparator-name (a b)
           (assert (typep a ,type)
                   nil
                   'type-error
                   :datum a
                   :expected-type ,type)
           (assert (typep b ,type)
                   nil
                   'type-error
                   :datum b
                   :expected-type ,type)
           ,@decls
           ,@body)
         (defun ,name (&rest items)
           ,docstring
           (declare (dynamic-extent items))
           (loop for head on items
                 while (cdr head)
                 do (multiple-value-bind (value surep)
                        (,pair-comparator-name (first head) (second head))
                      (unless surep (return (values nil nil)))
                      (unless value (return (values nil t))))
                 finally (return (values t t))))
         (define-compiler-macro ,name (&rest items)
           (let ((vars (loop
                         :for i :upfrom 0 :below (length items)
                         :collect (gensym (concatenate 'string "TIME-" (princ-to-string i) "-")))))
             `(let (,@(loop
                        :for var :in vars
                        :for item :in items
                        :collect (list var item)))
                ;; we could evaluate comparisons of timestamp literals here
                (and ,@(loop
                         :for (a b) :on vars
                         :while b
                         :collect `(,',pair-comparator-name ,a ,b))))))))))

(%defcomparator duration< ('duration)
  "Returns `(VALUES T T)` if every duration is shorter than the preceding
duration, else returns `(VALUES NIL T)`. May return `(VALUES NIL NIL)` if the
comparison is impossible to perform."
  (let ((value (%duration-compare a b)))
    (if (null value)
        (values nil nil)
        (values (eql value '<) t))))

(%defcomparator duration<= ('duration)
  "Returns `(VALUES T NIL)` if every duration is shorter than or equal to the
preceding duration, else returns `(VALUES NIL T)`. May return `(VALUES NIL NIL)`
if the comparison is impossible to perform."
  (let ((value (%duration-compare a b)))
    (if (null value)
        (values nil nil)
        (values (not (null (member value '(< =)))) t))))

(%defcomparator duration> ('duration)
  "Returns `(VALUES T NIL)` if every duration is longer than the preceding
duration, else returns `(VALUES NIL T)`. May return `(VALUES NIL NIL)`
if the comparison is impossible to perform."
  (let ((value (%duration-compare a b)))
    (if (null value)
        (values nil nil)
        (values (eql value '>) t))))

(%defcomparator duration>= ('duration)
  "Returns `(VALUES T NIL)` if every duration is longer than or equal to the
preceding duration, else returns `(VALUES NIL T)`. May return `(VALUES NIL NIL)`
if the comparison is impossible to perform."
  (let ((value (%duration-compare a b)))
    (if (null value)
        (values nil nil)
        (values (not (null (member value '(> =)))) t))))

(%defcomparator duration= ('duration)
  "Returns `(VALUES T NIL)` if every duration is equally long, else returns
`(VALUES NIL T)`. May return `(VALUES NIL NIL)` if the comparison is
impossible to perform."
  (let ((value (%duration-compare a b)))
    (if (null value)
        (values nil nil)
        (values (eql value '=) t))))

(%defcomparator duration/= ('duration)
  "Returns `(VALUES T NIL)` if every duration is not equally long, else returns
`(VALUES NIL T)`. May return `(VALUES NIL NIL)` if the comparison is
impossible to perform."
  (let ((value (%duration-compare a b)))
    (if (null value)
        (values nil nil)
        (values (not (eql value '=)) t))))
