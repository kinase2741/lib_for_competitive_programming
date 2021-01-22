;; BOF

(defpackage :rolling-hash-table
  (:nicknames :rhs)
  (:use :cl)
  (:export :make-rolling-hash-table
           :get-val
           :count-substrings
           :get-lowest-common-prefix))


(in-package :rolling-hash-table)

(defconstant +modulo+ (1- (ash 1 61)))
(defparameter *base* 1007)

(defstruct (rolling-hash-table (:conc-name rhs-)
                               (:constructor %make-rhs))
  (hash nil :type (simple-array fixnum 1))
  (pow nil :type (simple-array fixnum 1))
  (length nil :type fixnum))

(declaim (ftype (function (string) rolling-hash-table) make-rolling-hash-table))
(defun make-rolling-hash-table (string)
  (let ((n (length string)))
    (let ((rhs (%make-rhs :hash (make-array (1+ n)
                                            :element-type 'fixnum
                                            :adjustable nil
                                            :initial-element 0)
                          :pow (make-array (1+ n)
                                           :element-type 'fixnum
                                           :adjustable nil
                                           :initial-element 1)
                          :length n)))
      (loop for i of-type fixnum below n
            do (setf (aref (rhs-hash rhs) (1+ i)) (rem (+ (* (aref (rhs-hash rhs) i)
                                                              *base*)
                                                           (char-code (char string i)))
                                                       +modulo+)
                     (aref (rhs-pow rhs) (1+ i)) (rem (* (aref (rhs-pow rhs) i)
                                                         *base*)
                                                      +modulo+)))
      rhs)))

(defmethod get-val ((rhs rolling-hash-table)
                        (l fixnum)
                        (r fixnum))
  (let ((res (- (aref (rhs-hash rhs) r)
                 (rem (* (aref (rhs-hash rhs) l)
                         (aref (rhs-pow rhs) (- r l)))
                      +modulo+))))
    (if (minusp res)
        (+ res +modulo+)
        res)))

(defmethod count-substrings ((mainstr string)
                             (substr string))
  (let* ((rhs-main (make-rolling-hash-table mainstr))
         (rhs-sub (make-rolling-hash-table substr))
         (n (rhs-length rhs-main))
         (m (rhs-length rhs-sub)))
    (loop for i of-type fixnum
            from 0 to (- n m)
          with h1 of-type fixnum = (get-val rhs-sub 0 m)
          
          for h2 of-type fixnum = (get-val rhs-main i (+ i m))
          when (= h1 h2)
            count i)))

(defmethod get-lowest-common-prefix ((idx1 fixnum)
                                     (idx2 fixnum)
                                     (rhs rolling-hash-table))
  (labels ((%get-lcp (ok ng)
             (if (<= (abs (- ok ng)) 1)
                 ok
                 (let ((mid (ash (+ ok ng) -1)))
                   (if (= (get-val rhs idx1 (+ idx1 mid))
                          (get-val rhs idx2 (+ idx2 mid)))
                       (%get-lcp mid ng)
                       (%get-lcp ok mid))))))
    (%get-lcp 0 (- (rhs-length rhs) (max idx1 idx2)))))


(in-package :cl-user)

;; EOF
