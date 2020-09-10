(declaim (ftype (function (integer) list) int->lst))
(defun int->lst (int)
  (labels ((inner (k &optional (acc nil))
             (if (zerop k)
                 acc
                 (inner (truncate k 10)
                        (cons (rem k 10)
                              acc)))))
    (cond
      ((zerop int) '(0))
      ((minusp int) nil)
      (t (inner int)))))

;; Œ…”‚É’ˆÓ

(declaim (ftype (function (list) integer) lst->int))
(defun lst->int (list)
  (labels ((inner (xs &optional (acc 0))
             (if (null xs)
                 acc
                 (inner (rest xs) (+ (* acc 10)
                                     (first xs))))))
    (inner list)))
