(defpackage sqrt-tree
  (:use :cl)
  (:nicknames :st)
  (:export #:build
           #:fold
           #:update))

(in-package #:st)

(defstruct (sqrt-tree (:conc-name st-)
                      (:constructor %make-st))
  (main nil :type (simple-array fixnum (*)))
  (op-acc nil :type (simple-array fixnum (*)))
  (update-lazy nil :type (simple-array fixnum (*)))
  (k 0 :type fixnum)
  (op nil :type function)
  (e 0 :type fixnum))

(defun build (size &key (op #'+) (e 0))
  (let* ((k (isqrt size))
         (sub-size (ceiling size k)))
    (flet ((%make-arr (size)
             (make-array size :element-type 'fixnum
                              :initial-element e)))
      (%make-st :main (%make-arr size)
                :op-acc (%make-arr sub-size)
                :update-lazy (%make-arr sub-size)
                :k k
                :op op
                :e e))))

(defun %size (st)
  (length (st-main st)))

(defun %sub-idx (st idx)
  (with-slots (k) st
    (floor idx k)))

(defun %%propagate! (st i)
  (with-slots (main update-lazy) st
    (setf (aref main i)
          (aref update-lazy (%sub-idx st i)))))

(defun %propagate! (st idx)
  (with-slots (update-lazy k e) st
    (let* ((sub-idx (%sub-idx st idx))
           (idx-begin (* sub-idx k))
           (idx-end (max (+ idx-begin k))))
      ;; 初期値でなければ伝搬する
      (unless (= e (aref update-lazy sub-idx))
        (loop for i from idx-begin below idx-end
              do (%%propagate! st i))
        (setf (aref update-lazy sub-idx)
              e)))))

(defun lazy-p (st sub-idx)
  (with-slots (update-lazy e) st
    (/= e (aref update-lazy sub-idx))))

(defun %propagate-lazy (st idx)
  (with-slots (update-lazy k e) st
    (let* ((sub-idx (%sub-idx st idx)))
      (when (lazy-p st sub-idx)
        (%propagate! st idx)))))

(defun %update-main! (st idx value)
  (setf (aref (st-main st)
              idx)
        value))

(defun %propagate-op-acc! (st idx)
  ;; idxの該当するブロックに対応するop-accを更新
  (with-slots (main op-acc k e op) st
    (let* ((sub-idx (%sub-idx st idx))
           (idx-begin (* k sub-idx))
           (idx-end (max (%size st) (+ idx-begin k)))
           (tmp e))
      (loop for i from idx-begin below idx-end
            ;; 伝搬済みのはずなのでmainをみればOK
            for value = (aref main i)
            do (setf tmp
                     (funcall op tmp i)))
      (setf (aref op-acc sub-idx) tmp))))

(defmacro while (test &body body)
  `(loop while ,test
         do ,@body))

(defun update (st l r value)
  "[l,r)をvalueで更新する"
  (with-slots (k) st
    (let ((begin l)
          (end r)
          (ll (* k (ceiling l k)))
          (rr (* k (floor r k))))
      (%propagate-lazy st begin)
      (%propagate-lazy st end)
      (while (and (< l ll)
                  (< l r))
        (%update-main! st l value)
        (incf l))
      (%propagate-op-acc! st begin)
      (while (and (< rr r)
                  (< l r))
        (decf r)
        (%update-main! st r value))
      (%propagate-op-acc! st end)
      (while (< l r)
        (%update-op-acc! st l value)
        (incf l k)))))

(defun fold (st l r)
  (with-slots (main k e op op-acc) st
    (let ((begin l)
          (end r)
          (ll (* k (ceiling l k)))
          (rr (* k (floor r k)))
          (res e))
      (flet ((%update-res! (value)
               (setf res
                     (funcall op res value))))
        (%propagate-lazy st begin)
        (%propagate-lazy st end)
        (while (and (< l ll)
                    (< l r))
          (%update-res! (aref main l))
          (incf l))
        (while (and (< rr r)
                    (< l r))
          (decf r)
          (%update-res! (aref main r)))
        (while (< l r)
          (%update-res! (aref op-acc l))
          (incf l k))))))
