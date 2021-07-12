(defpackage sqrt-tree
  (:use :cl)
  (:nicknames :st))

(defstruct (sqrt-tree (:conc-name st-)
                      (:constructor %make-st))
  (main nil
   :type (simple-array fixnum (*)))
  (sub nil
   :type (simple-array fixnum (*)))
  (op nil :type function))

(defun build (size &Key (op #'+) (initial-element 0) (identity-element 0))
  (unless op
    (error "OP must be supplied."))
  (let* ((sub-size (1+ (isqrt size))))
    (%make-st :main (make-array size :element-type 'fixnum
                                               :initial-element initial-element)
              :sub (make-array sub-size :element-type 'fixnum
                                        :initial-element identity-element)
                        :op op)))

(defun fold (st l r)
  nil)

(defun update (st l r)
  nil)
