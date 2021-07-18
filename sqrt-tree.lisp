(defpackage sqrt-tree
  (:use :cl)
  (:nicknames :st)
  (:export #:build
           #:fold
           #:update))

(in-package #:st)

(defstruct (sqrt-tree (:conc-name st-)
                      (:constructor %make-st))
  (main nil
   :type (simple-array fixnum (*)))
  (sub nil
   :type (simple-array fixnum (*)))
  (lazy nil
   :type (simple-array boolean (*)))
  (k nil :type fixnum)
  (op nil :type function)
  (initial-element nil :type fixnum)
  (identity-element nil :type fixnum))

(defun propagate (st idx)
  "main[idx]の値を使う前に呼ぶ関数
   sub[idx//k]で遅延している値をmainに反映する
   sub[idx//k]はresetされる"
  (declare (sqrt-tree st)
           ((integer 0 #.most-positive-fixnum) idx))
  (with-slots (main sub lazy k identity-element) st
    (let* ((sub-idx (floor idx k))
           (new-value (aref sub sub-idx))
           (idx-begin (* sub-idx k))
           (idx-end (min (length main) (* (1+ sub-idx) k))))
      (when (aref lazy sub-idx)
        (flet ((%propagate! (index)
                 (setf (aref main index) new-value)))
          (loop for i from idx-begin below idx-end
                do (%propagate! i))
          (setf (aref sub sub-idx) identity-element))))))

(defmethod print-object ((obj sqrt-tree)
                         s)
  ;; TODO st->listの実装
  (fresh-line s)
  (princ "#SQRT-TREE" s)
  (princ (st->list obj) s))

(defun build (size &Key (op #'+) (initial-element 0) (identity-element 0))
  (unless op
    (error "OP must be supplied."))
  (let* ((k (isqrt size))
         (sub-size (1+ k)))
    (%make-st :main (make-array size :element-type 'fixnum
                                     :initial-element initial-element)
              :sub (make-array sub-size :element-type 'fixnum
                                        :initial-element identity-element)
              :lazy (make-array sub-size :element-type 'boolean
                                         :initial-element nil)
              :k k
              :op op
              :initial-element initial-element
              :identity-element identity-element)))

(defmacro while (test &body body)
  `(loop while ,test
         do ,@body))

(defun fold (st l r)
  (declare (sqrt-tree st)
           ((integer 0 #.most-positive-fixnum) l r))
  (with-slots (main sub k op identity-element) st
    (let ((res identity-element))
      (declare (fixnum res))
      (propagate st l)
      (while (and (< l r)
                  (not (zerop (rem l k))))
        (setf res
              (funcall op res (aref main l)))
        (incf l))
      (propagate st (1- r))
      (while (and (< l r)
                  (not (zerop (rem r k))))
        (decf r)
        (setf res (funcall op res (aref main r))))
      (while (< l r)
        (setf res (funcall op res (aref sub (floor l k))))
        (incf l k))
      res)))

(defun update (st value l r)
  (declare (sqrt-tree st)
           ((integer 0 #.most-positive-fixnum) l r))
  (with-slots (main sub (m k)) st
    (propagate st l)
    (while (and (< l r)
                (not (zerop (rem l m))))
      (setf (aref main l) value)
      (incf l))
    (propagate st (1- r))
    (while (and (< l r)
                (not (zerop (rem r m))))
      (decf r)
      (setf (aref main r) value))
    (while (< l r)
      (setf (aref sub (floor l m)) value)
      (incf l m))))
