(defun lower-bound (predicate lo hi)
  (if (<= (abs (- lo hi) ) 1)
      lo
      (let ((mid (truncate (+ lo hi) 2)))
        (if (funcall predicate mid)
            (lower-bound predicate mid hi)

            (lower-bound predicate lo mid)))))
