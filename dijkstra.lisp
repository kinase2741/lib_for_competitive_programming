;;;
;;; BOF
;;;

(defstruct state
  (cost -1 :type fixnum)
  (node -1 :type fixnum))

;; Binary heap (1-based)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symb (&rest args)
    (intern
     (with-output-to-string (s)
       (dolist (x args)
         (princ (string-upcase x) s))))))

;; TODO :
;; - heap-full-p
;; - invoke error when attempting to insert item to full heap


(defmacro define-binary-heap (struct-name &key element-type predicate key-fn key-type init)
  (let* ((name-str (symbol-name struct-name))
         (constructor (symb "make-" name-str))
         (empty-p (symb name-str "-empty-p"))
         (peek (symb name-str "-peek"))
         (push (symb name-str "-push"))
         (heapify-up (symb name-str "-heapify-up"))
         (pop (symb name-str "-pop"))
         (heapify-down (symb name-str "-heapify-down")))

    `(progn

       (defstruct (,struct-name (:constructor ,constructor (size))
                                (:copier nil))
         ;;
         ;; Example: TODO
         ;;
         (data (make-array (the fixnum (1+ size)) :element-type ',element-type :adjustable nil :initial-element ,init) :type (simple-array ,element-type (*)))
         (count 0 :type fixnum))

       (declaim (inline ,empty-p ,push ,heapify-up ,pop ,heapify-down ,peek))
       (defun ,empty-p (heap)
         (declare (,struct-name heap))
         (with-slots (data count) heap
           (declare (ignorable data))
           (zerop count)))

       (defun ,heapify-up (heap node-index)
         (declare (,struct-name heap)
                  (fixnum node-index))
         (with-slots (data count) heap
           (declare (ignorable count))
           (loop do
             (let ((parent-index (floor node-index 2)))
               (declare (fixnum parent-index))
               (labels ((ordered-p (parent child)
                          (declare (fixnum parent child))
                          (the boolean
                               (,predicate (the ,key-type (,key-fn (aref data parent)))
                                           (the ,key-type (,key-fn (aref data child)))))))
                 (when (or (<= parent-index 0)
                           (ordered-p parent-index
                                      node-index))
                   (return))
                 (rotatef (the ,element-type (aref data node-index))
                          (the ,element-type (aref data parent-index)))
                 (setf node-index parent-index))))))

       (defun ,heapify-down (heap root-index)
         (declare (,struct-name heap)
                  (fixnum root-index))
         (with-slots (data count) heap
           (declare (ignorable count))
           (loop do
             (let ((root root-index)
                   (l (* root-index 2))
                   (r (1+ (* root-index 2))))
               (declare (fixnum root l r))
               (labels ((ordered-p (p c)
                          (declare (fixnum p c))
                          (the boolean
                               (,predicate (the ,key-type (,key-fn (aref data p)))
                                           (the ,key-type (,key-fn (aref data c))))))
                        (swap! (x y)
                          (rotatef (the ,element-type (aref data x))
                                   (the ,element-type (aref data y)))))
                 (cond ((and (<= l count)
                             (or (> r count)
                                 (ordered-p l r))
                             (not (ordered-p root l)))
                        (swap! root l)
                        (setf root-index l))
                       ((and (<= r count)
                             (not (ordered-p root r)))
                        (swap! root r)
                        (setf root-index r))
                       (t (return))))))))

       (defun ,push (heap item)
         (declare (,struct-name heap)
                  (,element-type item))
         (with-slots (data count) heap
           (incf (the fixnum count))
           (setf (aref data count) item)
           (,heapify-up heap count)))

       (defun ,peek (heap)
         (declare (,struct-name heap))
         (with-slots (data count) heap
           (declare (ignorable count))
           (aref data 1)))

       (defun ,pop (heap)
         (declare (,struct-name heap))
         (with-slots (data count) heap
           (when (,empty-p heap)
             (error "Heap is empty."))
           (let ((res (aref data 1)))
             (declare (,element-type res))
             (prog1 res
               (setf (aref data 1)
                     (the ,element-type (aref data count)))
               (decf (the fixnum count))
               (,heapify-down heap 1))))))))

;; e.g. : for Dijkstra algorithm
;; (cost node)

(define-binary-heap heap
  :element-type (or null state)
  :predicate <
  :key-fn state-cost
  :key-type fixnum
  :init nil)


(defun dijkstra (start edges &key
                               (node-amount (length edges))
                               (init-value #.(expt 10 12)))
  ;; start: 始点
  ;; edges:
  ;;   edges[node]にはnodeから伸びる辺のリストを格納する
  ;;   edgeは(cost, next-node) の形であることを期待する
  ;; 帰り値:
  ;;   startから各nodeへの移動コストを格納した配列
  (declare (fixnum start node-amount)
           ((simple-array list (*)) edges))
  (let ((costs (make-array node-amount
                           :element-type 'fixnum
                           :initial-element init-value))
        (heap (make-heap 1000000))) ;; 競プロ用なのでサイズは適当
    (declare ((simple-array fixnum (*)) costs)
             (heap heap))
    (heap-push heap (make-state :cost 0
                                :node start))
    (loop while (not (heap-empty-p heap))
          do (with-slots (cost node)
                 (the state (heap-pop heap))
               (declare (fixnum cost node))
               (when (<= cost (aref costs node))
                 (setf #1=(aref costs node)
                       cost)
                 (loop for (dc next) of-type (fixnum fixnum) in (aref edges node)
                       do (let ((nc (+ cost dc)))
                            (declare (fixnum nc))
                            (when (< nc #2=(aref costs next))
                              (setf #2# nc)
                              (heap-push heap (make-state :cost nc
                                                          :node next))))))))
    costs))

;;;
;;; EOF
;;;
