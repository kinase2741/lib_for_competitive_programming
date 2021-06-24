(ql:quickload  :rove :slient t)

(defpackage :test/treap
  (:use #:cl)
  (:import-from #:rove))

(in-package  :test/treap)

(rove:deftest conversion
  (testing "Convert atom treap"
    (flet ((convert (list)
             (treap->list (list->treqp (list x))))))
    (ok (equal (convert (list 1))
               (list 1)))))

(rove:run-suite *package*)
