(defstruct (deque (:constructor make-deq)
                  (:conc-name deq-))
  (right nil :type list)
  (left nil :type list))

(defmethod deq-empty-p ((d deque))
  (and (null (deq-right d))
       (null (deq-left d))))

(defmethod deq-l-first ((d deque))
  "dequeの左端の要素を返す. 参照は共有される. dequeが空の場合はエラーとなる."
  (cond ((deq-empty-p d) (error "Deque is empty."))
        ((null (deq-left d)) (first (reverse (deq-right d))))
        (t (first (deq-left d)))))

(defmethod deq-r-first ((d deque))
  "dequeの右端の要素を返す. 参照は共有される. dequeが空の場合はエラーとなる."
  (cond ((deq-empty-p d) (error "Deque is empty."))
        ((null (deq-right d)) (first (reverse (deq-left d))))
        (t (first (deq-right d)))))


(defmethod deq-l-rest ((d deque))
  "dequeの左端から二番目以降の要素が入ったdequeを生成して返す.リストの参照は共有される."
  (cond ((deq-empty-p d) (error "Deque is empty."))
        ((null (deq-left d))
         (make-deq :left (rest (reverse (deq-right d)))
                   :right nil))
        (t (make-deq :left (rest (deq-left d))
                     :right (deq-right d)))))


(defmethod deq-r-rest ((d deque))
  "dequeの右端から二番目以降の要素が入ったdequeを生成して返す.リストの参照は共有される."
  (cond ((deq-empty-p d) (error "Deque is empty."))
        ((null (deq-right d))
         (make-deq :left nil
                   :right (rest (reverse (deq-right d)))))
        (t (make-deq :left (deq-left d)
                     :right (rest (deq-right d))))))


(defmethod deq-l-cons (item (d deque))
  "itemをdequeの左端に加えたものを返す.リストの参照は共有される."
  (make-deq :left (cons item (deq-left d))
            :right (deq-right d)))


(defmethod deq-r-cons (item (d deque))
  "itemをdequeの右端に加えたものを返す.リストの参照は共有される."
  (make-deq :left (deq-left d)
            :right (cons item (deq-right d))))
