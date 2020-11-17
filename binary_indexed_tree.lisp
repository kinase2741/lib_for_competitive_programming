(defparameter n 8)

(defparameter *bit*
  (make-array 64 :initial-element 0))

(defun last-digit-in-bit (x)
  (logand x (- x)))


(defun add (bit a w)
  "incf v w"
  (loop
     with x = a
     while (<= x n)
     do
       (incf (aref bit x)
             w)
       (incf x (last-digit-in-bit x))
     finally
       (return bit)))

(defun sum (bit a)
  (loop
     with res = 0
     with x = a
     while (plusp x)
     do
       (incf res (aref bit x))
       (decf x (last-digit-in-bit x))
     finally
       (return res)))


(defclass binary-indexed-tree ()
  ((dat :initarg :dat
        :accessor bit-dat)
   (size :initarg :size
         :accessor bit-size)))


(defun bit-create (size &optional (default 0))
  (make-binary-indexed-tree
   :dat (make-array (1+ size) :initial-element default)
   :size size))

(defmethod add ((bit binary-indexed-tree)
                (a fixnum))
  (labels ((add-sub (bit x)
             (if (<= x (binary-indexed-tree-size)))))))
