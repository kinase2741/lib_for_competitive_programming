(ql:quickload :fiveam :slient t)
(pushnew :rove *features*)

(defpackage :test/treap
  (:use #:cl
        #:treap)
  (:import-from #:fiveam))

(in-package :test/treap)

#-nil
(rove:ok (equal (treap::treap->list (treap::list->treap (list 1)))
                (list 1)))

#-nil
(rove:ok (equal (treap::treap->list (treap::list->treap (list 2)))
                (list 2)))
