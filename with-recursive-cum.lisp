(defmacro with-recursive-cum ((arr &optional (fn 'gcd) (n `(length ,arr)) (array-init -1) (res-init 0)) &body body)
  (let ((memo-l (gensym))
        (memo-r (gensym)))
    `(let ((,memo-l (make-array ,n :initial-element ,array-init))
           (,memo-r (make-array ,n :initial-element ,array-init)))
       (labels ((calc-l (i)
                  (if (not (<= 0 i (1- ,n)))
                      ,res-init
                      (let ((val (aref ,memo-l i)))
                        (if (/= ,array-init
                                (aref ,memo-l i))
                            val
                            (setf (aref ,memo-l i)
                                  (funcall ,fn
                                           (aref ,arr i)
                                           (calc-l (1- i))))))))
                (calc-r (i)
                  (if (not (<= 0 i (1- ,n)))
                      ,res-init
                      (let ((val (aref ,memo-r i)))
                        (if (/= ,array-init
                                (aref ,memo-r i))
                            val
                            (setf (aref ,memo-r i)
                                  (funcall ,fn
                                           (aref ,arr i)
                                           (calc-r (1+ i)))))))))
         ,@body))))










