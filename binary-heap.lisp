;;;
;;; BOF
;;;

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
  :element-type list
  :predicate <
  :key-fn first
  :key-type fixnum
  :init nil)


;;;
;;; EOF
;;;
