(ql:quickload :rove :slient t)

(defpackage :test/treap
  (:use #:cl
        #:treap)
  (:import-from #:rove))

(in-package :test/treap)

(let ((rove:*enable-colors* nil))
  (let ((l (treap::make-treap 1 :sum 1))
        (r (treap::make-treap 2 :sum 2))
        (xx (treap::list->treap (list 2 1)))
        (yy (treap::list->treap (list 4 3)))
        (ex1 (treap::list->treap (list 1 4 2 3)))
        (xs (list 2 4 3 1 4)))
    (prove:is (treap::%plus-cnt l r) 2)
    (prove:is (treap::%plus-sum l r) 3)
    (prove:is (treap::%plus-cnt l nil) 1)
    (prove:is (treap::%plus-sum l nil) 1)
    ;; merge
    (prove:is (treap::treap->list (treap::merge xx yy))
              '(2 1 4 3))

    ;; split
    (prove:is (multiple-value-bind (l r)
                  (treap::split ex1 2)
                (list (treap::treap->list l)
                      (treap::treap->list r)))
              '((1 4)
                (2 3)))

    ;; conversion
    (prove:is (treap::treap->list (treap::list->treap xs))
              xs)
    ;; insert
    (prove:is (treap::treap->list
               (treap::insert
                (treap::insert nil 0 11)
                1
                10))
              '(11 10))
    ;; remove
    (prove:is (treap::treap->list (treap::remove l 0))
              nil)
    (prove:is (treap::treap->list
               (treap::remove ex1 2))
              '(1 4 3))
    ;; ref
    (prove:is (treap::ref l 1) 1)
    (prove:is (treap::ref r 1) 2)))
