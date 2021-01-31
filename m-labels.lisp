(defmacro m-labels (((fn-name args &body fn-body)) &body body)
  (let ((val (gensym))
        (memo (gensym)))
    `(let ((,memo (make-hash-table :test #'equal)))
       (labels ((,fn-name ,args
                  (let ((,val (gethash (list  ,@args) ,memo)))
                    (when ,val (return-from ,fn-name ,val))
                    (setf (gethash (list ,@args) ,memo)
                          (progn
                            ,@fn-body)))))
         ,@body))))
