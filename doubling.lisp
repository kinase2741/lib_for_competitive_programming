;;;
;;; BOF
;;;


(defmacro with-doubling ((getter count dist first-lambda get-lambda &key (init 0) (element-type 'fixnum)) &body body)
  ;; (getter cnt start)
  (let ((log-count (gensym "LOG-COUNT"))
        (dists (gensym "DISTS"))
        (dub (gensym "DUB"))
        (amount (gensym))
        (pos (gensym))
        (n (gensym))
        (nn (gensym))
        (k (gensym))
        (i (gensym)))
    `(let* ((,amount (length ,dist))
            (,log-count (loop while (<= (ash 1 ,log-count) ,count)
                              with ,log-count of-type fixnum = 1
                              do (incf (the fixnum ,log-count))
                              finally (return ,log-count)))
            (,dists (make-array (list ,log-count ,amount) :element-type 'fixnum))
            (,dub (make-array (list ,log-count ,amount) :element-type ',element-type :initial-element ,init)))
       (declare (fixnum ,log-count)
                ((simple-array fixnum (* *)) ,dists)
                ((simple-array ,element-type (* *)) ,dub))
       (loop for ,i of-type fixnum below ,amount
             for ,n of-type fixnum = (aref ,dist ,i)
             do (setf (aref ,dists 0 ,i) ,n
                      (aref ,dub 0 ,i) (the ,element-type (,first-lambda ,i))))
       (loop for ,k of-type fixnum below (1- ,log-count)
             do (loop for ,pos of-type fixnum below ,amount
                      for ,n of-type fixnum = (aref ,dists ,k ,pos)
                      for ,nn of-type fixnum = (aref ,dists ,k ,n)
                      do (setf (aref ,dists (1+ ,k) ,pos) ,nn)
                      do (setf (aref ,dub (1+ ,k) ,pos)
                               (the ,element-type (,get-lambda (aref ,dub ,k ,pos)
                                                               (aref ,dub ,k ,n))))))
       (flet ((,getter (cnt start)
                (declare (fixnum cnt start))
                (loop for ,k of-type fixnum
                        below ,log-count
                      with res of-type ,element-type = ,init
                      with pos of-type fixnum = start
                      when (zerop cnt)
                        return (values res pos)
                      when (logbitp 0 cnt)
                        do (setf res (,get-lambda res
                                                  (aref ,dub ,k pos))
                                 pos (aref ,dists ,k pos))
                           
                      do (setf cnt (ash cnt -1))
                      finally (return (values res pos)))))
         ,@body))))

;;;
;;; EOF
;;;
