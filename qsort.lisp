(defmethod qsort ((sequence array))
  (labels ((swap (arr x y)
             (rotatef (aref arr x)
                      (aref arr y)))
           (qsort-sub (arr left right)
             (let ((l left)
                   (r right)
                   (pivot (aref arr (+ left
                                       (random (- right left))))))
               (loop while (<= l r) do
                    (loop while (< (aref arr l) pivot) do
                         (incf l))
                    (loop while (> (aref arr r) pivot) do
                         (decf r))
                    (when (<= l r)
                      (swap arr l r)
                      (incf l)
                      (decf r)))
               (when (< left r)
                 (qsort-sub arr left r))
               (when (< l right)
                 (qsort-sub arr l right)))))
    (qsort-sub sequence 0 (1- (length sequence)))
    sequence))
