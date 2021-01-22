;;;
;;; Beginning of inserted contents
;;;


(defmacro make-uf (size)
  (let ((parents (gensym)))
    `(let ((,parents (make-array ,size :element-type 'fixnum :adjustable nil :initial-element -1)))
       (labels ((%find (x)
                  (declare (fixnum x))
                  (the fixnum
                       (if (minusp (aref ,parents x))
                           x
                           (setf (aref,parents x)
                                 (%find (aref ,parents x))))))
                (%unite (x y)
                  (declare (fixnum x y))
                  (let ((x (%find x))
                        (y (%find y)))
                    (declare (fixnum x y))
                    (when (> x y)
                      (rotatef x y))
                    (when (/= x y)
                      (incf (aref ,parents x) (aref ,parents y))
                      (setf (aref ,parents y) x))))
                (%inquire (x y)
                  (declare (fixnum x y))
                  (= (aref ,parents x)
                     (aref ,parents y))))
         (lambda (key &optional (x -1) (y -1))
           (declare (symbol key)
                    (fixnum x y))
           (ecase key
             (:find (%find x))
             (:unite (%unite x y))
             (:inquire (%inquire x y))
             (:show ,parents)))))))

;;;
;;; End of inserted contents
;;;
