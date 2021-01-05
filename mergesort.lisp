(defmacro define-merge-sort (element-type initial-element)
  `(defun merge-sort! (array &key (predicate #'<) key)
     "Destructively sort array."
     (declare ((simple-array ,element-type 1) array)
              (function predicate))
     (let ((n (length array)))
       (declare (fixnum n))
       (flet ((compare (x y)
                (funcall predicate
                         (if key
                             (funcall key (aref array x))
                             (aref array x))
                         (if key
                             (funcall key (aref array y))
                             (aref array y)))))
         (flet ((merge-proc (i1 i2 j1 j2)
                  (when (and (< i1 n)
                             (< i2 n)
                             (plusp (- i2 i1))
                             (plusp (- (min j2 n) j1)))
                    (loop with j2 of-type fixnum = (min j2 n)
                          with li of-type fixnum = (- i2 i1)
                          with lj of-type fixnum = (- j2 j1)
                          with tmp of-type (simple-array ,element-type 1) =  (make-array (+ li lj)
                                                                                         :element-type ',element-type
                                                                                         :adjustable nil
                                                                                         :initial-element ,initial-element)
                          with i of-type fixnum = 0
                          with j of-type fixnum = 0
                          with k of-type fixnum = 0
                          while (and (< i li)
                                     (< j lj))
                          if (compare (+ i1 i)
                                      (+ j1 j))
                            do (setf (aref tmp k)
                                     (aref array (+ i1 i)))
                            and do (incf k)
                            and do (incf i)
                          else
                            do (setf (aref tmp k)
                                     (aref array (+ j1 j)))
                            and do (incf k)
                            and do (incf j)
                          finally
                             (if (< j lj)
                                 (loop for l of-type fixnum
                                         below (- j2 j1 j)
                                       do (setf (aref tmp (+ k l))
                                                (aref array (+ j1 j l))))
                                 (loop for l of-type fixnum
                                         below (- i2 i1 i)
                                       do (setf (aref tmp (+ k l))
                                                (aref array (+ i1 i l)))))
                             (loop for i of-type fixnum
                                     below (+ li lj)
                                   do (setf (aref array (+ i1 i))
                                            (aref tmp i)))))))
           (loop with k of-type fixnum = 1
                 while (< k n)
                 do (loop for i of-type fixnum
                            below n
                          by (* k 2)
                          do (merge-proc i
                                         (+ i k)
                                         (+ i k)
                                         (+ i (* k 2))))
                 do (setq k (* k 2))))))))

(define-merge-sort fixnum 0)
