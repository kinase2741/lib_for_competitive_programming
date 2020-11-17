(defun product (Xs &optional (repeat 1))
  (let ((res nil))
    (labels ((product-sub (xs &optional (acc nil) (cnt 0))
               (if (= cnt repeat)
                   (push (reverse acc) res)
                   (loop for x in xs do
                        (product-sub xs (cons x acc) (1+ cnt))))))
      (product-sub xs)
      (reverse res))))
