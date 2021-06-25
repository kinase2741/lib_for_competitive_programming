(ql:quickload :prove :slient t)
(pushnew :rove *features*)

(defpackage :test/treap
  (:use #:cl
        #:treap)
  (:import-from #:fiveam))

(in-package :test/treap)

(let ((prove:*enable-colors* nil))
  (let ((l (treap::make-treap 1 :sum 1))
        (r (treap::make-treap 2 :sum 2)))
    (prove:is 2 (treap::%get-cnt l r))
    (prove:is 3 (treap::%get-sum l r))))

#+nil
(rove:ok (equal (treap::treap->list (treap::list->treap (list 1)))
                (list 1)))

#+nil
(rove:ok (equal (treap::treap->list (treap::list->treap (list 2)))
                (list 2)))
