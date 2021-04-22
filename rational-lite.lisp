(defstruct (rational* (:conc-name r-)
                      (:constructor make-rational* (num denom &aux (g (gcd num denom)))))
  (p (floor num g) :type fixnum)
  (q (floor denom g) :type fixnum))


(defun rational*->rational (rational*)
  (declare (rational* rational*))
  #+swank (assert (not (zerop r-q rational*)))
  (the rational
       (/ (r-p rational*)
          (r-q rational*))))

(defun add (rat1 rat2)
  (declare (rational* rat1 rat2))
  (let* ((new-p (+ (* (r-q rat2)
                      (r-p rat1))
                   (* (r-p rat2)
                      (r-q rat1))))
         (new-q (* (r-q rat1)
                   (r-q rat2)))
         (g (gcd new-p new-q))
         (new-p (floor new-p g))
         (new-q (floor new-q g)))
    (declare (fixnum new-p new-q g))
    (make-rational* new-p new-q)))

(defun sub (rat1 rat2)
  (declare (rational* rat1 rat2))
  (add rat1
       (make-rational* (- (r-p rat2))
                       (r-q rat2))))

(defun mul (rat1 rat2)
  (declare (rational* rat1 rat2))
  (let* ((p (* (r-p rat1)
               (r-p rat2)))
         (q (* (r-q rat1)
               (r-q rat2)))
         (g (gcd p q))
         (new-p (floor p g))
         (new-q (floor q g)))
    (declare (fixnum p q g new-p new-q))
    (make-rational* new-p new-q)))


(defun div (rat1 rat2)
  (declare (rational* rat1 rat2))
  #+swank (assert (not (zerop (r-p rat2))))
  (mul rat1
       (make-rational* (r-q rat2)
                       (r-p rat2))))
