(declaim (ftype (function ((array * 2)) (array * 2)) copy-matrix))
(defun copy-matrix (matrix)
  (let ((new-mat (make-array (array-dimensions matrix)
                             :element-type (array-element-type matrix)
                             :adjustable (adjustable-array-p matrix))))
    (loop for i of-type fixnum below (array-dimension matrix 0)
          do (loop for j of-type fixnum below (array-dimension matrix 1)
                   do (setf (aref new-mat i j)
                            (aref matrix i j))))
    new-mat))

(declaim (ftype (function ((array * 2)) (array * 2)) rotate-matrix))
(defun rotate-matrix (matrix)
  " Return a matrix two-dimensional array rotated 90 degrees."
  (let ((m (array-dimension matrix 0))
        (n (array-dimension matrix 1)))
    (let ((new-mat (make-array (list n m)
                               :element-type (array-element-type matrix)
                               :adjustable (adjustable-array-p matrix))))
      (loop for i of-type fixnum below m
            do (loop for j of-type fixnum below n
                     do (setf (aref new-mat j (- m i 1))
                              (aref matrix i j))))
      new-mat)))

(declaim (ftype (function ((array * 2)) null) rotate-matrix!))
(defun rotate-matrix! (matrix)
  " Destructively rotate MATRIX."
  (destructuring-bind (m n) (array-dimensions matrix)
    (loop for i of-type fixnum below n
          do (loop for j of-type fixnum below m
                   do (rotatef (aref matrix j (- n i 1))
                               (aref matrix i j))))))

(declaim (ftype (function ((array * 2) (array * 2) &optional fixnum) (array * 2)) mat*))
(defun mat* (mat1 mat2 &optional modulo)
  (destructuring-bind (m n) (array-dimensions mat1)
    (destructuring-bind (p q) (array-dimensions mat2)
      (assert (= n p))
      (let ((new-mat (make-array (list m q)
                                 :element-type (array-element-type mat1)
                                 :adjustable (adjustable-array-p mat1))))
        (loop for i of-type fixnum below m
              do (loop for j of-type fixnum below q
                       do (setf (aref new-mat i j)
                                (loop for k of-type fixnum below n
                                      sum (if modulo
                                              (mod (* (aref mat1 i k)
                                                      (aref mat2 k j))
                                                   modulo)
                                              (* (aref mat1 i k)
                                                 (aref mat2 k j)))))))
        new-mat))))

(declaim (ftype (function ((array * 2) * &optional fixnum) (array * 2)) mat-pow))
(defun mat-pow (mat k &optional modulo)
  (destructuring-bind (m n) (array-dimensions mat)
    (assert (= m n))
    (let ((res (make-array (list m n)
                           :element-type (array-element-type mat)
                           :adjustable (adjustable-array-p mat)
                           :initial-element 0)))
      (loop for i of-type fixnum below n
            do (setf (aref res i i) 1))
      (sb-int:named-let rec ((k k)
                             (mat mat)
                             (res res))
        (cond ((<= k 0) res)
              ((oddp k) (rec (ash k -1)
                             (mat* mat mat modulo)
                             (mat* mat res modulo)))
              (t (rec (ash k -1)
                      (mat* mat mat modulo)
                      res)))))))
