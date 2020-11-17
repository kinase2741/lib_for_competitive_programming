;; Functions for prime

(declaim (inline prime-factorize-to-list))
(defun prime-factorize-to-list (integer)
  ;; 素因数分解分解してリストで返す(昇順)
  (declare ((integer 0) integer))
  (the list
       (if (<= integer 1)
           nil
           (loop
              while (<= (* f f) integer)
              with acc list = nil
              with f integer = 2
              do
                (if (zerop (rem integer f))
                    (progn
                      (push f acc)
                      (setq integer (floor integer f)))
                    (incf f))
              finally
                (when (/= integer 1)
                  (push integer acc))
                (return (reverse acc))))))

(declaim (inline prime-p))
(defun prime-p (integer)
  (declare ((integer 1) integer))
  (if (= integer 1)
      nil
      (loop
         with f = 2
         while (<= (* f f) integer)
         do
           (when (zerop (rem integer f))
             (return nil))
           (incf f)
         finally
           (return t))))

(defun enumerate-divisor (k)
  (if (= k 1)
      (list 1)
      (labels ((sub (k d acc)
                 (cond
                   ((> (* d d)
                       k)
                    (sort acc #'<))
                   ((zerop (rem k d))
                    (sub k
                         (1+ d)
                         (if (= (* d d)
                                k)
                             (cons d acc)
                             (cons d
                                   (cons (floor k d)
                                         acc)))))
                   (t
                    (sub k
                         (1+ d)
                         acc)))))
        (sub k 1 nil))))
