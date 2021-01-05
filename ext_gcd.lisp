(defun ext-gcd (a b x y)
  "ax + by = gcd(a,b) となるような (x,y) を求める"
  (if (zerop b)
      (values a 0 1 0)
      (multiple-value-bind (q r) (truncate a b)
        (ext-gcd b r (+ (* q x) y) x))))
