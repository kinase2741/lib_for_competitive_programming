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

(defun propagate (st idx))

(defun update (st l r))

(defun fold (st l r))
