(in-package #:cl-user)

(defpackage #:treap
  (:use #:cl)
  (:shadow #:merge
           #:remove
           #:ref))

(in-package  #:treap)

(defstruct (treap (:constructor make-treap (value &key (l nil) (r nil) (cnt 1) (sum value))))
  (value value)
  (l nil :type (or null treap))
  (r nil :type (or null treap))
  (priority (random #.most-positive-fixnum))  ;; 勝手に決まる
  (cnt cnt)
  (sum sum))

(defun treap->list (treap)
  "デバッグ用。O(n)"
  (let ((res nil))
    (labels ((%traverse (node)
               ;; 再帰的にpush
               (when node
                 (%traverse (treap-l node))
                 (push (treap-value node)
                       res)
                 (%traverse (treap-r node)))))
      (%traverse treap)
      (reverse res))))

(defun list->treap (list)
  "デバッグ用。O(n)"
  (let ((xs (copy-seq list)))
    (reduce (lambda (treap x)
              (merge treap (make-treap x :sum x)))
            xs
            :initial-value nil)))

(defun %get-cnt (treap)
  (if (null treap)
      0
      (treap-cnt treap)))

(defun %get-sum (treap)
  (if (null treap)
      0
      (treap-sum treap)))

(defun %plus-cnt (l r)
  (+ (%get-cnt l)
     (%get-cnt r)))

(defun %plus-sum (l r)
  (+ (%get-sum l)
     (%get-sum r)))

(defun merge (l r)
  (when (or (null l)
            (null r))
    (return-from merge (or l r)))
  (let ((new-cnt (%plus-cnt l r))
        (new-sum (%plus-sum l r)))
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

(defun split (treap key)
  "left:  k未満のnodeからなるtreap
   right: k以上のnodeからなるtreap"
  (cond
    ((null treap) (values nil nil))
    ((>= (%get-cnt (treap-l treap)) key)
     ;; cntが十分ある => 左
     (multiple-value-bind (new-l new-r)
         (split (treap-l treap)
                key)
       (let* ((r (merge (make-treap (treap-value treap)
                                    :sum (treap-value treap))
                        (treap-r treap)))
              (res-r (merge new-r r)))
         (values new-l res-r))))
    (:else
     ;; 右
     (let ((new-key (- key
                       (%get-cnt (treap-l treap)))))
       (multiple-value-bind (new-l new-r)
           (split (treap-r treap)
                  new-key)
         (let* ((l (merge (treap-l treap)
                          (make-treap (treap-value treap)
                                      :sum (treap-value treap))))
                (res-l (merge l
                              new-l)))
           (values res-l new-r)))))))

(defun insert (treap key value)
  ;; TODO: てすと
  (multiple-value-bind (l r)
      (split treap key)
    (merge (merge l (make-treap value :sum value))
           r)))

(defun remove (treap key)
  (multiple-value-bind (l c-r)
      (split treap (1- key))
    (multiple-value-bind (c r)
        (split c-r key)
      (let ((res (merge l r)))
        (values res c)))))

(defun ref (treap key)
  (multiple-value-bind (_removed center)
      (remove treap key)
    (declare (ignore _removed))
    (and center
         (treap-value center))))


#+swank
(rove:deftest test-treap
  (let* ((xs (loop repeat 5 collect (random 100)))
         (ys (loop repeat 10 collect (random 100)))
         (zs (loop repeat 20 collect (random 100)))
         (ws (list 1 3 5 7 10))
         (xs-treap (list->treap xs))
         (ys-treap (list->treap ys))
         (zs-treap (list->treap zs))
         (ws-treap (list->treap ws)))
    (rove:testing "Testing equality"
      (flet ((convert (list)
               (treap->list (list->treap list))))
        (rove:ok (equal (convert xs)
                        xs))))
    (rove:testing "get-cnt"
      (rove:ok (= (%get-cnt ys-treap)
                  10)))
    (rove:testing "get-sum"
      (rove:ok (= (%get-sum ws-treap)
                  26)))
    (rove:testing "plus-cnt"
      (rove:ok (= (%plus-cnt xs-treap
                             ys-treap)
                  15)))))

#+swank
(rove:run-suite *package*)

(in-package #:cl-user)
