(in-package #:cl-user)

(defpackage #:treap
  (:use #:cl))

(in-package  #:treap)


(in-package #:cl-user)

;; Load file to run tests

#+swank (load (merge-pathnames "test/treap.lisp" (truename ".")))
