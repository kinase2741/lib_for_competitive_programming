;; Utils to treat int in mod. (be sure to set +mod+)


(defconstant +mod+ 1000000007)

(declaim (ftype (function (finxum &optional fixnum) fixnum) modint))
(defun modint (x &optional (m +mod+))
  (declare (integer x))
  (cond
    ((and (>= x 0) (< x m)) x)
    ((minusp x) (modint (+ x m)))
    (t (mod x m))))

(declaim (ftype (function (&rest fixnum) fixnum) mod+))
(defun mod+ (&rest args)
  (declare (inline modint))
  (if (null args)
      0
      (modint (reduce #'+ args))))


(declaim (ftype (function (&rest fixnum) fixnum) mod-))
(defun mod- (&rest args)
  (declare (inline modint))
  (if (null args)
      0
      (modint (reduce #'- args))))

(declaim (ftype (function (&rest fixnum) fixnum) mod*))
(defun mod* (&rest args)
  (declare (inline modint))
  (if (null args)
      0
      (modint (reduce #'* args))))

(declaim (ftype (function (fixnum &optional fixnum) fixnum) mod-inv))
(defun mod-inv (a &optional (m +mod+))
  (declare (integer a m))
  (let ((b m)
        (u 1)
        (v 0))
    (loop until (zerop b) do
         (let ((w (truncate a b)))
           (decf a (* w b))
           (rotatef a b)
           (decf u (* w v))
           (rotatef u v))
       finally
         (loop while (minusp u) do
              (incf u m))
         (return (mod u m)))))

(declaim (ftype (function (&rest fixnum) fixnum) mod/))
(defun mod/ (&rest args)
  (declare (inline modint))
  (if (null args)
      0
      (reduce (lambda (x y)
                (modint (* x (mod-inv y))))
              args
              :initial-value 1)))

(declaim (ftype (function (finxum fixnum &optional fixnum) fixnum) mod-pow))
(defun mod-pow (a n &optional (m +mod+))
  (declare (integer a)
           ((integer 0) n))
  (labels ((sub (a n &optional (res 1))
             (if (zerop n)
                 res
                 (sub (mod (* a a) m)
                      (truncate n 2)
                      (if (oddp n)
                          (mod (* res a) m)
                          res)))))
    (sub a n)))


(declaim (ftype (function (fixnum fixnum &optional fixnum) fixnum) mod-binomial))
(defun mod-binomial (n k &optional (m +mod+))
  (declare ((integer 0) m))
  (if (or (< n k) (< n 0) (< k 0))
      0
      (let ((k (if (< k (- n k)) k (- n k)))
            (num 1)
            (denom 1))
        (declare ((integer 0) k num denom))
        (loop for x from n above (- n k) do
             (setq num (mod (* num x) m)))
        (loop for x from 1 to k do
             (setq denom (mod (* denom x) m)))
        (mod (* num (mod-inv denom m)) m))))
