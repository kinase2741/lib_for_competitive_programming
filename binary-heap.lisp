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

;;; TODO

(defmacro define-binary-heap (struct-name &key element-type predicate key-fn key-type init)
  (let* ((name-str (symbol-name struct-name))
         (constructor (symb "make-" name-str))
         (empty-p (symb name-str "-empty-p"))
         (peek (symb name-str "-peek"))
         (insert (symb name-str "-insert"))
         (heapify-up (symb name-str "-heapify-up"))
         (extract (symb name-str "-extract"))
         (heapify-down (symb name-str "-heapify-down")))
    
    `(progn
       
       (defstruct (,struct-name (:constructor ,constructor (size))
                                (:copier nil))
         ;; 
         ;; Example: TODO
         ;;
         (data (make-array (the fixnum (+ size 10)) :element-type ',element-type :adjustable nil :initial-element ,init) :type (simple-array ,element-type (*)))
         (count 0 :type fixnum))

       (declaim (inline ,empty-p))
       (defun ,empty-p (heap)
         (declare (,struct-name heap))
         (with-slots (data count) heap
           (declare (ignorable data))
           (zerop count)))

       (declaim (inline ,insert))
       (defun ,insert (heap item)
         (declare (,struct-name heap)
                  (,element-type item))
         (with-slots (data count) heap
           (incf (the fixnum count))
           (setf (aref data count) item)
           (,heapify-up heap count)))

       (defun ,heapify-up (heap node-index)
         (declare (,struct-name heap)
                  (fixnum node-index))
         (with-slots (data count) heap
           (declare (ignorable count))
           (let ((parent-index (floor node-index 2)))
             (declare (fixnum parent-index))
             (labels ((ordered-p (parent child)
                        (declare (fixnum parent child))
                        (the boolean
                             (,predicate (the ,key-type (,key-fn (aref data parent)))
                                         (the ,key-type (,key-fn (aref data child)))))))
               (when (and (plusp parent-index)
                          (not (ordered-p parent-index
                                          node-index)))
                 (rotatef (the ,element-type (aref data node-index))
                          (the ,element-type (aref data parent-index)))
                 (,heapify-up heap parent-index))))))

       (defun ,peek (heap)
         (declare (,struct-name heap))
         (with-slots (data count) heap
           (declare (ignorable count))
           (aref data 1)))

       (defun ,extract (heap)
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
               (,heapify-down heap 1)))))

       (defun ,heapify-down (heap root-index)
         (declare (,struct-name heap)
                  (fixnum root-index))
         (with-slots (data count) heap
           (declare (ignorable count))
           (let ((root root-index)
                 (l (* root-index 2))
                 (r (1+ (* root-index 2))))
             (declare (fixnum root l r))
             (labels ((ordered-p (p c)
                        (declare (fixnum p c))
                        (the boolean
                             (,predicate (the ,element-type (,key-fn (aref data p)))
                                         (the ,element-type (,key-fn (aref data c))))))
                      (swap! (x y)
                        (rotatef (the ,element-type (aref data x))
                                 (the ,element-type (aref data y)))))
               (cond ((and (<= l count)
                           (or (> r count)
                               (ordered-p l r))
                           (not (ordered-p root l)))
                      (swap! root l)
                      (,heapify-down heap l))
                     ((and (<= r count)
                           (not (ordered-p root r)))
                      (swap! root r)
                      (,heapify-down heap r))))))))))

(define-binary-heap heap
  :element-type fixnum
  :predicate <
  :key-fn identity
  :key-type fixnum
  :init 0)

;;;
;;; EOF
;;;
