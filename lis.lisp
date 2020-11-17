(defparameter *inf* 1000000000)

(defun count-lis (sequence &optional (n (length sequence)))
  (let ((dp (make-array (1+ n) :initial-element *inf*)))
    (labels ((lower-bound (item lo hi)
               (if (= (- hi lo) 1)
                   lo
                   (let ((mid (floor (+ lo hi)
                                     2)))
                     (if (< (aref dp mid) item)
                         (lower-bound item mid hi)
                         (lower-bound item lo mid))))))
      (map nil
           (lambda (x)
             (let ((point (lower-bound x -1 n)))
               (setf (aref dp (1+ point))
                     x)))
           sequence)
      (1+ (lower-bound *inf* -1 n)))))
