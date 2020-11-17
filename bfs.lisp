ize* 100)

(defstruct deque
  (data nil)
  (size nil)
  (head 0)
  (tail 0)
  (count 0))


(defun deque-create (&optional (size *default-deque-size*))
  (make-deque
   :size size
   :data (make-array size)))



(defmethod deque-clear ((d deque))
  (fill (deque-data d) 0)
  (setf (deque-size d) 0)
  (setf (deque-head d) 0)
  (setf (deque-tail d) 0)
  (setf (deque-count d) 0))

;; Subcommand

(declaim (inline deque-empty-p
                 deque-full-p
                 deque-get-prev-index
                 deque-get-next-index))


(defmethod deque-empty-p ((d deque))
  (zerop (deque-count d)))



(defmethod deque-full-p ((d deque))
  (= (deque-count d) (deque-size d)))

(defmethod deque-get-prev-index ((d deque) idx)
  (declare (inline deque-get-next-index))
  (if (zerop idx)
      (1- (deque-size d))
      (1- idx)))

(defmethod deque-get-next-index ((d deque) idx)
  (rem (1+ idx) (deque-size d)))




;;; Main command

(defmethod deque-pushfront ((d deque) item)
  (when (deque-full-p d)
    (error "deque is full"))

  (setf (deque-head d) (deque-get-prev-index d (deque-head d)))
  (setf (aref (deque-data d) (deque-head d)) item)
  (when (deque-empty-p d) ; first insersion
    (setf (deque-tail d) (deque-head d)))
  (incf (deque-count d)))



(defmethod deque-pushback ((d deque) item)
  (when (deque-full-p d)
    (error "deque is full"))
  
  (setf (deque-tail d) (deque-get-next-index d (deque-tail d)))
  (setf (aref (deque-data d) (deque-tail d)) item)
  (when (deque-empty-p d) ; first insersion
    (setf (deque-head d) (deque-tail d)))
  (incf (deque-count d)))


(defmethod deque-popfront ((d deque))
  (when (deque-empty-p d)
    (error "deque is empty,"))
  
  (let ((value (aref (deque-data d) (deque-head d))))
    (setf (deque-head d) (deque-get-next-index d (deque-head d)))
    (decf (deque-count d))
    value))



(defmethod deque-popback ((d deque))
  (when (deque-empty-p d)
    (error "deque is empty,"))
  
  (let ((value (aref (deque-data d) (deque-tail d))))
    (setf (deque-tail d) (deque-get-prev-index d (deque-tail d)))
    (decf (deque-count d))
    value))


(defmethod dref ((d deque) idx)
  (let ((arr (deque-data d))
        (size (deque-size d))
        (head (deque-head d)))
    (aref arr (mod (+ idx
                      head)
                   size))))

(defparameter *inf* 10000000)

(defparameter *dy-dx*
  '((1 . 0)
    (0 . 1)
    (-1 . 0)
    (0 . -1)))

(defun bfs (r c sy sx gy gx board)
  (declare (fixnum r c sy sx gy gx)
           ((array character 2) board))
  (let ((memo (make-array `(,r ,c) :element-type 'fixnum :adjustable nil :initial-element *inf*))
        (q (deque-create 10000)))
    (declare ((array fixnum 2) memo)
             (deque q))
    (deque-pushback q (cons sy sx))
    (setf (aref memo sy sx) 0)
    (loop until (deque-empty-p q) do
         (let* ((pos (deque-popfront q))
                (y (first pos))
                (x (rest pos)))
           (declare (fixnum y x))
           (when (and (= gy y)
                      (= gx x))
             (return (aref memo gy gx)))
           (loop for d in *dy-dx* do
                (let ((ny (+ y
                             (first d)))
                      (nx (+ x
                             (rest d))))
                  (when (and (<= 0 ny (1- r))
                             (<= 0 nx (1- c))
                             (char-equal (aref board ny nx) #\.)
                             (= (aref memo ny nx)
                                *inf*))
                    (deque-pushback q (cons ny nx))
                    (setf (aref memo ny nx)
                          (1+ (aref memo y x)))))))
       finally
         (return -1))))
