;;;
;;; Beginning of inserted contents
;;;

;; modint functions

(defconstant +mod+ 1000000007)
;; (defconstant +mod+ 998244353)

(declaim (inline modint)
         (ftype (function (integer &optional fixnum) fixnum) modint))
(defun modint (integer &optional (m +mod+))
  (declare (integer integer))
  (loop while (minusp integer)
        do (incf integer m))
  (the fixnum
       (if (< integer m)
           integer
           (mod integer m))))


(defmacro define-modulo-operation (fn-name op-long op-short)
  `(progn
     (declaim (ftype (function (&rest fixnum) fixnum) ,fn-name)
              (inline ,fn-name))
     (defun ,fn-name (&rest args)
       (reduce (lambda (x y)
                 ,op-long)
               (rest args)
               :initial-value (modint (first args))))

     (define-compiler-macro ,fn-name (&whole form &rest args)
       (if (< (length args) 10)
           (reduce (lambda (x y)
                     ,op-short)
                   (rest args)
                   :initial-value `(modint ,(first args)))
           form))))



(define-modulo-operation mod+ (modint (+ x (modint y))) `(modint (+ ,x (modint ,y))))
(define-modulo-operation mod- (modint (- x (modint y))) `(modint (- ,x (modint ,y))))
(define-modulo-operation mod* (modint (* x (modint y))) `(modint (* ,x (modint ,y))))
(define-modulo-operation mod/ (modint (* x (mod-inv y))) `(modint (* ,x (mod-inv ,y))))

(declaim (ftype (function (fixnum &optional fixnum) fixnum) mod-inv))
(defun mod-inv (a &optional (m +mod+))
       "Reference:https://qiita.com/drken/items/3b4fdf0a78e7a138cd9a"
       (declare (fixnum a))
       (let ((b m)
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

(declaim (ftype (function (fixnum (integer 0)) fixnum) mod-power)
         (inline mod-power))
(defun mod-power (base power)
  ;; Reference:https://qiita.com/drken/items/3b4fdf0a78e7a138cd9a
  (declare (fixnum base)
           ((integer 0) power))
  (loop while (plusp power)
        with res of-type fixnum = 1
        do (when (logbitp 0 power)
             (setf res (mod* res base)))
           (setf base (mod* base base))
           (setf power (ash power -1))
        finally
           (return res)))

(define-modify-macro incmodf (&optional (val 1)) (lambda (place val) (mod+ place val)))
(define-modify-macro decmodf (&optional (val 1)) (lambda (place val) (mod- place val)))
(define-modify-macro mulmodf (&optional (val 1)) (lambda (place val) (mod* place val)))
(define-modify-macro divmodf (&optional (val 1)) (lambda (place val) (mod/ place val)))

(declaim (ftype (function (fixnum) (simple-array fixnum 1)) make-mod-table)
         (inline make-mod-table))
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

(declaim (ftype (function (fixnum fixnum (simple-array fixnum 1)) fixnum) mod-combi)
         (inline mod-combi))
(defun mod-combi (n k table)
  (declare (fixnum n k)
           ((simple-array fixnum 1) table))
  (if (or (< n k)
          (< n 0)
          (< k 0))
      0
      (the fixnum
           (mod* (aref table n)
                 (mod-inv (aref table k))
                 (mod-inv (aref table (- n k)))))))



;;;
;;; End of inserted contents
;;;
