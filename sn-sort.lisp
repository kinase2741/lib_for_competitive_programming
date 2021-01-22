;; Sort Network (Ref: "Let Over Lambda" written by Doug Hoyte)
;; Array-size should be less than 100.



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun build-butcher-sn (n)
    (let* (network
           (tee (ceiling (log n 2)))
           (p (ash 1 (- tee 1))))
      (loop while (> p 0)
            do (let ((q (ash 1 (- tee 1)))
                     (r 0)
                     (d p))
                 (loop while (> d 0)
                       do (loop for i from 0 to (- n d 1)
                                do (when (= (logand i p) r)
                                     (push (cons i (+ i d)) network)))
                          (setf d (- q p)
                                q (ash q -1)
                                r p)))
               (setf p (ash p -1)))
      (nreverse network))))


(defmacro sn-sorter (array-length)
  (let ((arr (gensym)))
    `(lambda (,arr)
       (declare (optimize (speed 3) (safety 0) (debug 0)))
       (declare ((simple-array fixnum 1) ,arr))
       (tagbody 
          ,@(mapcar (lambda (pair)
                      `(let ((a #1=(aref ,arr ,(car pair)))
                             (b #2=(aref ,arr ,(cdr pair))))
                         (if (> a b)
                             (setf #1# b
                                   #2# a))))
                    (build-butcher-sn array-length)))
       ,arr)))
