(ql:quickload :prove :slient t)
(pushnew :prove *features*)

(defpackage :test/treap
  (:use #:cl
        #:treap)
  (:import-from #:rove))

(in-package :test/treap)

(let ((prove:*enable-colors* nil))
  (let ((l (treap::make-treap 1 :sum 1))
        (r (treap::make-treap 2 :sum 2)))
    (prove:is (treap::%get-cnt l r) 2)
    (prove:is (treap::%get-sum l r) 3)
    (prove:is (treap::%get-cnt l nil) 1)
    (prove:is (treap::%get-sum l nil) 1x)))

#+nil
(rove:ok (equal (treap::treap->list (treap::list->treap (list 1)))
                (list 1)))

#+nil
(rove:ok (equal (treap::treap->list (treap::list->treap (list 2)))
                (list 2)))
