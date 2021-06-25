(in-package #:cl-user)

(defpackage #:treap
  (:use #:cl))

(in-package  #:treap)

(defstruct (treap (:constructor make-treap (value &key (children (vector nil nil)) (cnt 1) (sum 0))))
  (value value)
  (children children :type t)
  (priority (random #.most-positive-fixnum))  ;; 勝手に決まる
  (cnt cnt)
  (sum sum))

(defun treap->list (treap) (list 1))

(defun list->treap (list) (make-treap 1))

(in-package #:cl-user)

;; Load file to run tests

#+swank (load (merge-pathnames "test/treap.lisp" (truename ".")))
