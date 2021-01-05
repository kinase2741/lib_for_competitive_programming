;;;
;;; BOF
;;;


(defmacro with-memoized-fn ((function-name (&rest args) requirements invalid-value  value-if-not-found convert-fn hash-test) &body body)
  (let ((memo (gensym))
        (val (gensym))
        (win (gensym)))
    `(let ((,memo (make-hash-table :test ,hash-test :size 100000)))
       (labels ((,function-name ,args
                  (multiple-value-bind (,val ,win)
                      (gethash (funcall ,convert-fn ,@args) ,memo)
                    (cond
                      (,win ,val)
                      ((not ,requirements) ,invalid-value)
                      (t
                       (setf (gethash (funcall ,convert-fn ,@args) ,memo)
                             ,value-if-not-found))))))
         ,@body))))

;;;
;;; EOF
;;;
