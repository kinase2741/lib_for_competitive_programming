(defpackage sqrt-tree
  (:use :cl)
  (:nicknames :st))

(in-package #:st)

(defstruct (sqrt-tree (:conc-name st-)
                      (:constructor %make-st))
  (main nil
   :type (simple-array fixnum (*)))
  (sub nil
   :type (simple-array fixnum (*)))
  (interval nil :type fixnum)
  (op nil :type function)
  (initial-element nil :type fixnum)
  (identity-element nil :type fixnum))

(defmethod print-object ((obj sqrt-tree)
                         s)
  (with-slots (main sub interval identity-element) obj
    (let ((n (length main))
          (init nil))
      (fresh-line s)
      (princ "#SQRT-TREE(" s)
      (dotimes (i n)
        (let* ((k (floor i interval))
               (sub-val (aref sub k)))
          (cond
            (init
             (princ #\Space s))
            (:else
             (setf init t)))
          (princ (if (eql sub-val identity-element)
                     (aref main i)
                     sub-val)
                 s)))
      (princ ")" s))))

(defun build (size &Key (op #'+) (initial-element 0) (identity-element 0))
  (unless op
    (error "OP must be supplied."))
  (let* ((interval (isqrt size))
         (sub-size (1+ interval)))
    (%make-st :main (make-array size :element-type 'fixnum
                                     :initial-element initial-element)
              :sub (make-array sub-size :element-type 'fixnum
                                        :initial-element identity-element)
              :interval interval              :op op
              :initial-element initial-element
              :identity-element identity-element)))

(defmacro while (test &body body)
  `(loop while ,test
         do ,@body))

(defun fold (st l r)
  (declare (sqrt-tree st)
           ((integer 0 #.most-positive-fixnum) l r))
  (with-slots (main sub interval op identity-element) st
    (let ((res identity-element))
      (declare (fixnum res))
      (while (and (< l r)
                  (not (zerop (rem l interval))))
        (setf res
              (funcall op res (aref main l)))
        (incf l))
      (while (and (< l r)
                  (not (zerop (rem r interval))))
        (decf r)
        (setf res (funcall op res (aref main r))))
      (while (< l r)
        (setf res (funcall op res (aref sub (floor l interval))))
        (incf l interval))
      res)))

(defun update (st value l r)
  (declare (sqrt-tree st)
           ((integer 0 #.most-positive-fixnum) l r))
  (with-slots (main sub (m interval)) st
    (while (and (< l r)
                (not (zerop (rem l m))))
      (setf (aref main l) value)
      (incf l))
    (while (and (< l r)
                (not (zerop (rem r m))))
      (decf r)
      (setf (aref main r) value))
    (while (< l r)
      (setf (aref sub (floor l m)) value)
      (incf l m))))
