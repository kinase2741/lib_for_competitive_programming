(defparameter *inf* 1000000000)

(defun count-lis (n vector)
  (loop for i from 1 below n
     with dp = (make-array n :initial-element *inf*)
     with cnt = 1
     initially
       (setf (aref dp 0) (aref vector 0))
     do
       (if (> (aref vector i) (aref dp (1- cnt)))
           (progn
             (setf (aref dp cnt)
                   (aref vector i))
             (incf cnt))
           (let ((ng -1)
                 (ok (1- n)))
             (loop while (> (abs (- ng ok)) 1)
                do
                  (let ((mid (floor (+ ng ok) 2)))
                    (if (< (aref vector i) (aref dp mid))
                        (setq ok mid)
                        (setq ng mid))))
             (setf (aref dp ok)
                   (aref vector i))))
     finally
       (return cnt)))
