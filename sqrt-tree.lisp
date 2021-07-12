(defpackage sqrt-tree
  (:use :cl)
  (:nicknames :st))

(defstruct (sqrt-tree (:conc-name st-)
                      (:constructor %make-st))
  (main nil
   :type (simple-array fixnum (*)))
  (sub nil
   :type (simple-array fixnum (*)))
  (sub-size nil :type fixnum)
  (op nil :type function)
  (initial-element nil :type fixnum)
  (identity-element nil :type fixnum))

(defun build (size &Key (op #'+) (initial-element 0) (identity-element 0))
  (unless op
    (error "OP must be supplied."))
  (let* ((sub-size (1+ (isqrt size))))
    (%make-st :main (make-array size :element-type 'fixnum
                                     :initial-element initial-element)
              :sub (make-array sub-size :element-type 'fixnum
                                        :initial-element identity-element)
              :sub-size sub-size
              :op op
              :initial-element initial-element
              :identity-element identity-element)))

(defmacro while (test &body body)
  `(loop while ,test
         do ,@body))

(defun fold (st l r)
  (declare (sqrt-tree st)
           ((integer 0 #.most-positive-fixnum) l r))
  (with-slots (main sub sub-size op identity-element) st
    (let ((res identity-element))
      (declare (fixnum res))
      (while (and (< l r)
                  (not (zerop (rem l sub-size))))
        (setf res
              (funcall op res (aref main l)))
        (incf l))
      (while (and (< l r)
                  (not (zerop (rem r sub-size))))
        (decf r)
        (setf res (funcall op res (aref main r))))
      (while (< l r)
        (setf res (funcall op res (aref sub (floor l sub-size))))
        (incf l sub-size))
      res)))

(defun update (st l r)
  nil)
