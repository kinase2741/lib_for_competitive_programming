;;;
;;; BOF
;;;

;; 座標圧縮

(declaim (inline compress get-unzipped-value))
(labels ((%bs (fn ok ng)
           (loop while (> (abs (- ok ng)) 1)
              for mid = (ash (+ ok ng) -1)
              if (funcall fn mid)
              do (setf ok mid)
              else do (setf ng mid)
              finally
                (return ok)))
         (%remove-duplicates (xs)
           (let ((memo (make-hash-table :test #'equal :size 100000))
                 (res (make-array (length xs) :adjustable (adjustable-array-p xs) :fill-pointer 0)))
             (map nil
                  (lambda (x)
                    (unless (gethash x memo)
                      (vector-push x res)))
                  xs)
             xs)))
  (declare (inline %bs))
  
  (defun compress (vector &optional (index-origin 0))
    (let* ((n (length vector))
           (zipped (make-array n :element-type (array-element-type vector)
                                 :adjustable (adjustable-array-p vector)))
           (unique (sort (%remove-duplicates vector) #'<))
           (m (length unique)))
      (loop for i below n
         for x = (aref vector i)
         do (setf (aref zipped i)
                  (+ (1+ (%bs (lambda (j)
                               (< (aref unique j) x))
                             -1
                             m))
                     index-origin))
         finally
           (return (values zipped unique)))))

  (defun get-unzipped-value (unique value)
    (aref unique value)))

;;;
;;; EOF
;;;
