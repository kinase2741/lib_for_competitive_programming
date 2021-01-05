;; Non-lazy square-root decomposition
;; 一点更新、区間max/min取得

(defpackage :sqrt-decomp
  (:nicknames :decomp)
  (:use :cl)
  (:export :make-decomp :update :get-val))

(in-package :sqrt-decomp)

(defstruct (sqrt-decomp (:conc-name decomp-)
                        (:constructor %make-decomp))
  (dat nil :type vector)
  (sub nil :type vector)
  (fn nil :type function)
  (e nil :type fixnum)
  (n nil :type fixnum)
  (m nil :type fixnum))

(defun make-decomp (vector &key (fn #'max) (e (expt 10 12)) (n (length vector)))
  (let ((m (isqrt n)))
    (let ((decomp (%make-decomp :dat vector
                                :sub (make-array (ceiling n m)
                                                 :adjustable nil
                                                 :element-type 'fixnum
                                                 :initial-element e)
                                :fn fn
                                :e e
                                :n n
                                :m m)))
      (dotimes (i n decomp)
        (setf (aref (decomp-sub decomp) (floor i m))
              (aref vector i))))))

(define-modify-macro fn-f (var fn) (lambda (place var fn) (funcall fn place var)))

(defmethod update ((i fixnum)
                   (val fixnum)
                   (decomp sqrt-decomp))
  (setf (aref (decomp-dat decomp) i)
        val)
  (fn-f (aref (decomp-sub decomp) (floor i (decomp-m decomp)))
        val
        (decomp-fn decomp)))


(defmethod get-val ((l fixnum)
                    (r fixnum)
                    (decomp sqrt-decomp))
  (let ((res (decomp-e decomp))
        (m (decomp-m decomp)))
    (macrolet ((dat-fn-f (idx)
                 `(fn-f res
                        (aref (decomp-dat decomp) ,idx)
                        (decomp-fn decomp)))
               (sub-fn-f (idx)
                 `(fn-f res
                        (aref (decomp-sub decomp) ,idx)
                        (decomp-fn decomp)))
               (while (test &body body)
                 `(loop while ,test do ,@body)))
      (while (and (< l r)
                  (not (zerop (rem l m))))
        (dat-fn-f l)
        (incf l))
      (while (and (< l r)
                  (not (zerop (rem r m))))
        (dat-fn-f (1- r))
        (decf r))
      (while (< l r)
        (sub-fn-f (floor l m))
        (incf l m))
      res)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :sqrt-decomp))

(in-package :cl-user)
