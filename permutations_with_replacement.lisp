(defun product (sequence &optional (repeat 1))
  (let ((res nil))
    (labels ((product-sub (xs &optional (acc nil) (cnt 0))
               (if (= cnt repeat)
                   (push (reverse acc) res)
                   (loop for x in xs do
                        (product-sub xs (cons x acc) (1+ cnt))))))
      (product-sub sequence)
      (reverse res))))

(defun combinations-with-replacement (sequence r)
  (labels ((sorted-p (xs)
             (if (null (rest xs))
                 t
                 (if (<= (first xs) (second xs))
                     (sorted-p (rest xs))
                     nil))))
    (remove-if-not #'sorted-p (product sequence r))))
