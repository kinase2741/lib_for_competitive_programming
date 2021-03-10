(declaim (ftype (function (Fixnum fixnum) fixnum) ext-gcd))
(defun ext-gcd (a b)
  (declare (fixnum a b)
           (optimize (speed 3) (safety 2)))
  (let ((x 1)
        (y 0)
        (u 0)
        (v 1))
    (declare (fixnum x y u v))
    (loop while (plusp b)
          for k of-type fixnum = (floor a b)
          do (psetf (the fixnum x) (the fixnum u)
                    (the fixnum y) (the fixnum v)
                    (the fixnum u) (the fixnum (- x (the fixnum (* k u))))
                    (the fixnum v) (the fixnum (- y (the fixnum (* k v))))
                    (the fixnum a) b
                    (the fixnum b) (mod a b)))
    (the (values fixnum fixnum)
         (values x y))))

(declaim (ftype (function (fixnum fixnum) fixnum) mod-inv))
(defun mod-inv (a m)
  (let ((x (ext-gcd a m)))
    (if (< x 0)
        (+ x m)
        (mod x m))))
