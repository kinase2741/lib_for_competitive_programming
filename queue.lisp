;; Queue

(defstruct queue
  (front nil :type list)
  (rear nil :type list))

(defmethod queue-empty-p ((q queue))
  (and (null (queue-front q))
       (null (queue-rear q))))

(defmethod queue-pop ((q queue))
  (when (queue-empty-p q)
    (error "queue is empty"))
  (when (null (queue-front q))
    (setf (queue-front q) (reverse (queue-rear q))
          (queue-rear q) nil))
  (pop (queue-front q)))

(defmethod queue-push (item (q queue))
  (push item (queue-rear q)))
