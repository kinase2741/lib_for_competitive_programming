(ql:quickload :prove :slient t)
(pushnew :prove *features*)

(defpackage :test/treap
  (:use #:cl
        #:treap)
  (:import-from #:rove))

(in-package :test/treap)

(let ((prove:*enable-colors* nil))
  (let ((l (treap::make-treap 1 :sum 1))
        (r (treap::make-treap 2 :sum 2))
        (xs (list 2 4 3 1 4)))
    (prove:is (treap::%plus-cnt l r) 2)
    (prove:is (treap::%plus-sum l r) 3)
    (prove:is (treap::%plus-cnt l nil) 1)
    (prove:is (treap::%plus-sum l nil) 1)
    ;; conversion
    (prove:is (treap::treap->list (treap::list->treap xs)) (sort (copy-seq xs) #'<))
    ;; insert
    (prove:is (treap::treap->list (treap::insert nil 0 10))
              (list 10))
    ;; remove
    (prove:is (treap::remove l 0) nil)
    (prove:is (treap::remove r 0) nil)
    ;; ref
    (prove:is (treap::ref l 0) 1)
    (prove:is (treap::ref r 0) 2)))

#+nil
(rove:ok (equal (treap::treap->list (treap::list->treap (list 1)))
                (list 1)))

#+nil
(rove:ok (equal (treap::treap->list (treap::list->treap (list 2)))
                (list 2)))
