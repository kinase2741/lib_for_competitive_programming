;; BOF

(defun combinations (sequence)
  (flet ((combi-sub (xs b &optional (acc nil))
           (loop for i below (length xs) do
                (when (oddp (ash b (- i)))
                  (push (first xs) acc))
                (pop xs)
              finally
                (return (reverse acc)))))
    (mapcar (lambda (b)
              (combi-sub sequence b))
            (loop for b below (ash 1 (length sequence)) collect b))))

;; EOF
