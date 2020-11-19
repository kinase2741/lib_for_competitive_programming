(defstruct queue
  (front nil :type list)
  (back nil :type list))

(defmethod queue-empty-p ((q queue))
  (and (null (queue-front q))
       (null (queue-back q))))

(defmethod queue-first ((q queue))
  "queueの最初の要素を返す. 参照は共有される. queueが空の場合はエラーとなる."
  (cond ((queue-empty-p q) (error "Queue is empty."))
        ((null (queue-front q)) (first (reverse (queue-back q))))
        (t (first (queue-front q)))))


(defmethod queue-rest ((q queue))
  "queueの二番目以降の要素が入ったqueueを生成して返す.リストの参照は共有される."
  (cond ((queue-empty-p q) nil)
        ((null (queue-front q))
         (make-queue :front (rest (reverse (queue-back q)))
                     :back nil))
        (t (make-queue :front (rest (queue-front q))
                       :back (queue-back q)))))


(defmethod queue-cons (item (q queue))
  "itemをqueueの末尾に加えたものを返す.リストの参照は共有される."
  (make-queue :front (queue-front q)
              :back (cons item (queue-back q))))
