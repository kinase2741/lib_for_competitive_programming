(defparameter *default-queue-size* 1000)

(defstruct queue
  (data nil)
  (head 0)
  (tail 0)
  (size 0)
  (count 0))

(defmethod queue-empty-p ((q queue))
  (zerop (queue-count q)))

(defmethod queue-full-p ((q queue))
  (= (queue-count q)
     (queue-size q)))

(defun queue-create (&optional (size *default-queue-size*))
  (make-queue :data (make-array size)
              :size size))

(defmethod queue-push ((q queue) item)
  (assert (not (queue-full-p q)))
  (setf (aref (queue-data q) (queue-tail q)) item)
  (when (zerop (queue-count q))
    (setf (queue-head q) (queue-tail q)))
  (setf (queue-tail q) (rem (1+ (queue-tail q))
                            (queue-size q)))
  (incf (queue-count q))
  item)

(defmethod queue-pop ((q queue))
  (assert (not (queue-empty-p q)))
  (let ((item (aref (queue-data q) (queue-head q))))
    (setf (queue-head q) (mod (1- (queue-head q))
                              (queue-size q)))
    (decf (queue-count q))
    item))

(defmethod queue-ref ((q queue) idx)
  (aref (queue-data q) (rem (+ (queue-head q) idx)
                            (queue-size q))))

