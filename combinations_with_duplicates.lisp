(defun combinations-with-duplicates (xs r)
  "リストxsから重複を許してr個の要素を取る組み合わせの集合を返す"
  (let ((res nil)
        (n (length xs)))
    (labels ((sub (xs len r acc)
               (cond
                 ((null xs) nil)
                 ((zerop r) (push (reverse acc) res))
                 (t
                  (loop for i below len do
                    (let ((xs (nthcdr i xs)))
                      (sub xs
                           (- len i)
                           (1- r)
                           (cons (first xs)
                                 acc))))))))
      (sub xs n r nil)
      (reverse res))))