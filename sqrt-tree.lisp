(defpackage sqrt-tree
  (:use :cl)
  (:nicknames :st)
  (:export #:build
           #:fold
           #:update))

(in-package #:st)

(defun build (size &key (op #'+) (e 0)))

(defun propagate (st idx))

(defun update (st l r))

(defun fold (st l r))
