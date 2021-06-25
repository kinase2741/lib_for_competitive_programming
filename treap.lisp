(in-package #:cl-user)

(defpackage #:treap
  (:use #:cl)
  (:shadow #:merge))

(in-package  #:treap)

(defstruct (treap (:constructor make-treap (value &key (l nil) (r nil) (cnt 1) (sum 0))))
  (value value)
  (l nil :type (or null treap))
  (r nil :type (or null treap))
  (priority (random #.most-positive-fixnum))  ;; 勝手に決まる
  (cnt cnt)
  (sum sum))

(defun treap->list (treap) (list 1))

(defun list->treap (list) (make-treap 1))

(defun %get-sum (l r)
  (+ (treap-cnt l)
     (treap-cnt r)))

(defun merge (l r)
  (let ((new-cnt (+ (treap-cnt l)
                    (treap-cnt r)))
        (new-sum (+ (treap-value l)
                    (treap-value r))))
    (if (> (treap-priority l)
           (treap-priority r))
        ;; lが上
        (make-treap (treap-value l)
                    :l (treap-l l)
                    :r (merge (treap-r l)
                              r)
                    :cnt new-cnt
                    :sum new-sum)
        ;; rが上
        (make-treap (treap-value r)
                    :l (merge l
                              (treap-l r))
                    :r (treap-r r)
                    :cnt new-cnt
                    :sum new-sum))))

(in-package #:cl-user)

;; Load file to run tests

#+swank (load (merge-pathnames "test/treap.lisp" (truename ".")))
