;;;
;;; BOF
;;;

(defconstant +modulo+ (1- (ash 1 61)))
(defconstant +base+ 1007)

(defstruct (rolling-hash-table (:conc-name rhs-)
                               (:constructor %make-rhs))
  (hash nil :type (simple-array fixnum 1))
  (pow nil :type (simple-array fixnum 1))
  (length nil :type fixnum))

(declaim (ftype (function (string) rolling-hash-table) make-rolling-hash-table))
(defun make-rolling-hash-table (string)
  (let ((n (length string)))
    (declare (fixnum n))
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
                                                             +base+)
                                                           (char-code (char string i)))
                                                       +modulo+)
                     (aref (rhs-pow rhs) (1+ i)) (rem (* (aref (rhs-pow rhs) i)
                                                         +base+)
                                                      +modulo+)))
      rhs)))

(defun get-val (rhs l r)
  (declare (rolling-hash-table rhs)
           (fixnum l r))
  (let ((res (- (aref (rhs-hash rhs) r)
                 (rem (* (aref (rhs-hash rhs) l)
                         (aref (rhs-pow rhs) (- r l)))
                      +modulo+))))
    (declare (fixnum res))
    (the fixnum
         (if (minusp res)
             (+ res +modulo+)
             res))))

(defun count-substrings (mainstr substr)
  (declare (string mainstr substr))
  (let* ((rhs-main (make-rolling-hash-table mainstr))
         (rhs-sub (make-rolling-hash-table substr))
         (n (rhs-length rhs-main))
         (m (rhs-length rhs-sub)))
    (declare (rolling-hash-table rhs-main rhs-sub)
             (fixnum n m))
    (the fixnum
         (loop for i of-type fixnum
               from 0 to (- n m)
               with h1 of-type fixnum = (get-val rhs-sub 0 m)
               
               for h2 of-type fixnum = (get-val rhs-main i (+ i m))
               when (= h1 h2)
                 count i))))

(defun get-lowest-common-prefix (idx1 idx2 rhs)
  (declare (fixnum idx1 idx2)
           (rolling-hash-table rhs))
  (labels ((%get-lcp (ok ng)
             (declare (fixnum ok ng))
             (the fixnum
                  (if (<= (abs (the fixnum (- ok ng))) 1)
                      ok
                      (let ((mid (ash (+ ok ng) -1)))
                        (declare (fixnum mid))
                        (if (= (get-val rhs idx1 (the fixnum (+ idx1 mid)))
                               (get-val rhs idx2 (the fixnum (+ idx2 mid))))
                            (%get-lcp mid ng)
                            (%get-lcp ok mid)))))))
    (%get-lcp 0 (- (rhs-length rhs) (max idx1 idx2)))))


;;;
;;; EOF
;;;
