(defstruct queue front back)

(defun make-empty-queue ()
  (make-queue :front nil
              :back nil))

(defmethod queue-empty-p ((q queue))
  (and (null (queue-front q))
       (null (queue-back q))))

(defmethod head ((q queue))
  (cond ((queue-empty-p q) nil)
        ((null (queue-front q)) (first (reverse (queue-back q))))
        (t (first (queue-front q)))))


(defmethod tail ((q queue))
  (cond ((queue-empty-p q) nil)
        ((null (queue-front q))
         (make-queue :front (rest (reverse (queue-back q)))
                     :back nil))
        (t (make-queue :front (rest (queue-front q))
                       :back (queue-back q)))))


(defmethod queue-cons (item (q queue))
  (make-queue :front (queue-front q)
              :back (cons item (queue-back q))))
