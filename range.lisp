(defun range (start end &optional (step 1))
                                        ; generate range list
  (declare (type integer start)
           (type integer step))
  (when (or
         (zerop step)
         (and (> start end)
              (plusp step))
         (and (< start end)
              (minusp end)))
    (error "invalid arguments"))
  (labels ((range-sub (start end step &optional (acc nil))
             (if (or
                  (and (>= start end)
                       (plusp step))
                  (and (<= start end)
                       (minusp end)))
                 (reverse acc)
                 (range-sub (+ start step) end step (cons start acc)))))
    (range-sub start end step)))