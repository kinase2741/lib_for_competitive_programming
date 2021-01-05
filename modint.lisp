;;; modint functions

(defmacro define-modulo-functions (m)
  `(progn

     (declaim (ftype (function (integer) fixnum) modint))
     (defun modint (integer)
       (cond ((> integer ,m) (mod integer ,m))
             ((minusp integer) (modint (+ integer ,m)))
             (t integer)))
     
     (declaim (ftype (function (&rest fixnum) fixnum) mod+))
     (defun mod+ (&rest args)
       (reduce (lambda (x y) (modint (+ x y))) args))

     (declaim (ftype (function (&rest fixnum) fixnum) mod-))
     (defun mod- (&rest args)
       (reduce (lambda (x y) (modint (- x y))) args))
     
     
     (declaim (ftype (function (&rest fixnum) fixnum) mod*))
     (defun mod* (&rest args)
       (reduce (lambda (x y) (modint (* x y))) args))
     
     
     (declaim (ftype (function (fixnum) fixnum) mod-inv))
     (defun mod-inv (a)
       "Reference:https://qiita.com/drken/items/3b4fdf0a78e7a138cd9a"
       (declare (fixnum a))
       (let ((b ,m)
             (u 1)
             (v 0))
         (declare (fixnum b u v))
         (loop until (zerop b) do
           (let ((w (truncate a b)))
             (decf a (* w b))
             (rotatef a b)
             (decf u (* w v))
             (rotatef u v))
               finally
                  (return (modint u)))))

     (declaim (ftype (function (&rest fixnum) fixnum) mod/))
     (defun mod/ (&rest args)
       (reduce (lambda (x y) (modint (* x (mod-inv y)))) args))

     (declaim (ftype (function (fixnum (integer 0)) fixnum) mod-power))
     (defun mod-power (base power)
       "Reference:https://qiita.com/drken/items/3b4fdf0a78e7a138cd9a"
       (declare (fixnum base)
                ((integer 0) power))
       (loop while (plusp power)
             with res of-type fixnum = 1
             do (when (logbitp 0 power)
                  (setf res (mod* res base)))
                (setf base (* base base))
                (setf power (ash power -1))
             finally
                (return res)))


     (define-modify-macro incmodf (&optional (val 1)) (lambda (place val) (mod+ place val)))
     (define-modify-macro decmodf (&optional (val 1)) (lambda (place val) (mod- place val)))
     (define-modify-macro mulmodf (&optional (val 1)) (lambda (place val) (mod* place val)))
     (define-modify-macro divmodf (&optional (val 1)) (lambda (place val) (mod/ place val)))
     



     (declaim (ftype (function (fixnum) (simple-array fixnum 1)) make-mod-table))
     (defun make-mod-table (size)
       (let ((table (make-array (1+ size)
                                :element-type 'fixnum
                                :adjustable nil)))
         (setf (aref table 0) 1)
         (dotimes (i size)
           (setf (aref table (1+ i))
                 (mod* (aref table i)
                       (1+ i))))
         table))

     (declaim (ftype (function (fixnum fixnum (simple-array fixnum 1)) fixnum) mod-combi))
     (defun mod-combi (n k table)
       (if (or (< n k)
               (< n 0)
               (< k 0))
           0
           (mod* (aref table n)
                 (mod-inv (aref table k))
                 (mod-inv (aref table (- n k))))))))



(defconstant +mod+ 1000000007)

(define-modulo-functions +mod+)
