;;;
;;; Beginning of inserted contents
;;;

;; modint functions

;; modの値をここで定義する

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *mod* 1000000007)
  ;; (defparameter *mod* 998244353)
  )

(deftype mint () `(unsigned-byte 32))

(declaim (inline modint)
         (ftype (function (integer) mint) modint))
(defun modint (integer)
  "整数を引数に取り*mod*で割ったあまりを返す"
  ;; (integer) -> (mint)
  (declare (integer integer))
  (loop while (minusp integer)
        do (incf integer *mod*))
  (the mint
       (if (< integer *mod*)
           integer
           (mod integer *mod*))))

(declaim (ftype (function (mint) mint) mod-inv))
(defun mod-inv (a)
  "Reference:https://qiita.com/drken/items/3b4fdf0a78e7a138cd9a"
  (declare (mint a)
           (optimize (speed 3) (safety 2)))
  (let ((b *mod*)
        (u 1)
        (v 0))
    (declare (fixnum b u v))
    (loop until (zerop b) do
      (let ((w (truncate a b)))
        (declare (fixnum w))
        (decf a (the fixnum (* w b)))
        (rotatef a b)
        (decf u (the fixnum (* w v)))
        (rotatef u v)))
    (modint u)))

(defmacro define-modulo-operation (fn-name op-long op-short)
  `(progn
     ;; (&REST mint) -> (mint)
     (declaim (ftype (function (&rest mint) mint) ,fn-name)
              (inline ,fn-name))
     (defun ,fn-name (&rest args)
       (reduce (lambda (x y)
                 ,op-long)
               (rest args)
               :initial-value (first args)))

     (define-compiler-macro ,fn-name (&whole form &rest args)
       (if (< (length args) 10)
           (reduce (lambda (x y)
                     ,op-short)
                   (rest args)
                   :initial-value (first args))
           form))))

(define-modulo-operation mod+ (modint (+ x y)) `(modint (+ ,x ,y)))
(define-modulo-operation mod- (modint (- x y)) `(modint (- ,x ,y)))
(define-modulo-operation mod* (modint (* x y)) `(modint (* ,x ,y)))
(define-modulo-operation mod/ (modint (* x (mod-inv y))) `(modint (* ,x (mod-inv ,y))))

(declaim (ftype (function (mint (integer 0)) mint) mod-power)
         (inline mod-power))
(defun mod-power (base power)
  ;; Reference:https://qiita.com/drken/items/3b4fdf0a78e7a138cd9a
  (declare (mint base)
           ((integer 0) power))
  (loop while (plusp power)
        with res of-type mint = 1
        do (psetq base (the mint (mod* base base))
                  power (the (integer 0) (ash power -1))
                  res (the mint (if (logbitp 0 power)
                                    (mod* res base)
                                    res)))
        finally (return res)))

(define-modify-macro incmodf (&optional (val 1)) (lambda (place val) (mod+ place val)))
(define-modify-macro decmodf (&optional (val 1)) (lambda (place val) (mod- place val)))
(define-modify-macro mulmodf (&optional (val 1)) (lambda (place val) (mod* place val)))
(define-modify-macro divmodf (&optional (val 1)) (lambda (place val) (mod/ place val)))

(declaim (ftype (function (mint) (simple-array mint (*))) make-mod-table)
         (inline make-mod-fact-table))
(defun make-mod-fact-table (size)
  (declare (mint size))
  (let ((table (make-array (1+ size)
                           :element-type 'mint)))
    (declare ((simple-array mint (*)) table))
    (setf (aref table 0) 1)
    (loop for i of-type fixnum below size
          do (setf (aref table (1+ i))
                   (mod* (aref table i)
                         (the mint (1+ i)))))
    table))

(declaim (ftype (function (mint mint (simple-array mint (*))) mint) mod-combi-with-table)
         (inline mod-combi-with-table))
(defun mod-combi-with-table (n k table)
  (declare (mint n k)
           ((simple-array mint (*)) table))
  (the mint
       (if (or (< n k)
               (< n 0)
               (< k 0))
           0
           (mod* (aref table n)
                 (mod-inv (aref table k))
                 (mod-inv (aref table (the mint (- n k))))))))


;;;
;;; End of inserted contents
;;;
