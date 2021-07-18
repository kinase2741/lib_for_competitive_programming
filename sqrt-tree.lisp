(defpackage sqrt-tree
  (:use :cl)
  (:nicknames :st)
  (:export #:build
           #:fold
           #:update))

(in-package #:st)

(defstruct (sqrt-tree (:conc-name st-)
                      (:constructor %make-st))
  (main nil :type (simple-array fixnum (*)))
  (op-acc nil :type (simple-array fixnum (*)))
  (update-lazy nil :type (simple-array fixnum (*)))
  (k 0 :type fixnum)
  (op nil :type function)
  (e 0 :type fixnum))

(defun build (size &key (op #'+) (e 0))
  (let* ((k (isqrt size))
         (sub-size (ceiling size k)))
    (flet ((%make-arr (size)
             (make-array size :element-type 'fixnum
                              :initial-element e)))
      (%make-st :main (%make-arr size)
                :op-acc (%make-arr sub-size)
                :update-lazy (%make-arr sub-size)
                :k k
                :op op
                :e e))))

(defun %size (st)
  (length (st-main st)))

(defun %sub-idx (st idx)
  (with-slots (k) st
    (floor idx k)))

(defun %%propagate! (st i)
  (with-slots (main update-lazy) st
    (setf (aref main i)
          (aref update-lazy (%sub-idx st i)))))

(defun %propagate! (st idx)
  (with-slots (update-lazy k e) st
    (let* ((sub-idx (%sub-idx st idx))
           (idx-begin (* sub-idx k))
           (idx-end (max (+ idx-begin k))))
      ;; 初期値でなければ伝搬する
      (unless (= e (aref update-lazy sub-idx))
        (loop for i from idx-begin below idx-end
              do (%%propagate! st i))
        (setf (aref update-lazy sub-idx)
              e)))))

(defun %propagate (st idx)
  (with-slots (update-lazy k e) st
    (let* ((sub-idx (%sub-idx st idx)))
      (unless (= e (aref update-lazy sub-idx))
        (%propagate! st idx)))))

(defmacro while (test &body body)
  `(loop while ,test
         do ,@body))

(defun update (st l r value)
  "[l,r)をvalueで更新する"
  (with-slots (k) st
    (let ((begin l)
          (end r)
          (ll (* k (ceiling l k)))
          (rr (* k (floor r k))))
      (%propagate st begin)
      (%propagate st end)
      (while (and (< l ll)
                  (< l r))
        (%update-main! st l value)
        (incf l))
      (%propagate-op-acc! st begin value)
      (while (and (< rr r)
                  (< l r))
        (decf r)
        (%udpate-main! st r value))
      (%propagate-op-acc! st end value)
      (while (< l r)
        (%update-op-acc! st l value)
        (incf l)))))

(defun fold (st l r))
