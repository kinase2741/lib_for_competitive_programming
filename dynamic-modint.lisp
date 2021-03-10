;;;
;;; BOF
;;;

;; Dynamic modint


(defmacro with-modint-functions (modulo &body body)
  "Introduce modint functions in an anaphoric way."
  ;; (modint x)
  ;; (mod+ x y)
  ;; (mod- x y)
  ;; (mod* x y)
  ;; (mod-inv x)
  ;; (mod/ x y)
  ;; (mod-power base power)
  `(labels ((modint (x)
              (loop while (minusp x)
                    do (incf x ,modulo)
                    finally (return (the fixnum (mod x ,modulo)))))
            (mod+ (&rest args)
              (reduce (lambda (x y)
                        (modint (+ x (modint y))))
                      (rest args)
                      :initial-value (modint (first args))))
            (mod- (&rest args)
              (reduce (lambda (x y)
                        (modint (- x (modint y))))
                      (rest args)
                      :initial-value (modint (first args))))
            (mod* (&rest args)
              (reduce (lambda (x y)
                        (modint (* x (modint y))))
                      (rest args)
                      :initial-value (modint (first args))))
            (ext-gcd (a b)
              (if (zerop b)
                  (values 1 0 a)
                  (multiple-value-bind (q r) (truncate a b)
                    (multiple-value-bind (x y d) (ext-gcd b r)
                      (let ((n y)
                            (m (- x (* q y))))
                        (values n m d))))))
            (mod-inv (x)
              (modint (ext-gcd x ,modulo)))
            (mod/ (&rest args)
              (reduce (lambda (x y)
                        (modint (* x (mod-inv y))))
                      (rest args)
                      :initial-value (modint (first args))))
            (mod-power (base power)
              (sb-int:named-let rec ((base (modint base))
                                     (power power)
                                     (res 1))
                (if (zerop power)
                    res
                    (rec (mod* base base)
                         (ash power -1)
                         (if (logbitp 0 power)
                             (mod* res base)
                             res)))))
            (mod-combi (n k)
              (when (zerop n)
                (return-from mod-combi 0))
              (loop for r from 1 to k
                    for s from n downto 0
                    with res = 1
                    do (setf res (mod* res
                                       s))
                       (setf res (mod/ res
                                       r))
                    finally (return res))))
     ,@body))

;;;
;;; EOF
;;;
