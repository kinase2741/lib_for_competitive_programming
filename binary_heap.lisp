;;; Binary heap (1-based)
;;; This is port of https://yottagin.com/?p=8807.html.


(defclass binary-heap ()
  ;; 
  ;; Example: TODO
  ;;
  ((heap :type 'array
         :initarg :heap
         :accessor heap)
   (predicate :initarg :predicate
              :reader heap-predicate)
   (heap-count :initarg :count
               :accessor heap-count)
   (heap-max-size :initarg :max-size
                  :accessor heap-max-size)))

(defun heap-create (max-size &key (predicate #'<) (initial-element nil))
  (make-instance 'binary-heap
                 :heap (make-array (1+ max-size)
                                   :initial-element initial-element
                                   :adjustable nil)
                 :predicate predicate
                 :count 0
                 :max-size max-size))

(defmethod heap-empty-p ((heap binary-heap))
  (zerop (heap-count heap)))


(defmethod heap-full-p ((heap binary-heap))
  (= (heap-count heap)
     (heap-max-size heap)))

(defmethod heap-insert ((heap binary-heap) item)
  (when (heap-full-p heap)
    (error "Heap is full."))
  (incf (heap-count heap))
  (setf (aref (heap heap) (heap-count heap))
        item)
  (heapify-up heap (heap-count heap)))

(defmethod heapify-up ((heap binary-heap) index)
  (let ((parent-index (floor index 2)))
    (when (and (plusp parent-index)
               (funcall (heap-predicate heap)
                        (aref (heap heap) index)
                        (aref (heap heap) parent-index)))
      (rotatef (aref (heap heap) index)
               (aref (heap heap) parent-index))
      (heapify-up heap parent-index))))

(defmethod heap-peek ((heap binary-heap))
  (aref (heap heap) 1))

(defmethod heap-extract ((heap binary-heap))
  (when (heap-empty-p heap)
    (error "Heap is empty."))
  (prog1 (heap-peek heap)
    (rotatef (aref (heap heap) 1)
             (aref (heap heap) (heap-count heap)))
    (decf (heap-count heap))
    (heapify-down heap 1)))

(defmethod heapify-down ((heap binary-heap) index)
  (let* ((index-left-child (* index 2))
         (index-right-child (1+ (* index 2)))
         (root-index index))
    (when (and (<= index-left-child (heap-count heap))
               (<= index-right-child (heap-count heap)))
      (when (funcall (heap-predicate heap)
                     (aref (heap heap) index-right-child)
                     (aref (heap heap) root-index))
        (setf root-index index-right-child))
      (when (funcall (heap-predicate heap)
                     (aref (heap heap) index-left-child)
                     (aref (heap heap) root-index))
        (setf root-index index-left-child))
      (when (/= root-index index)
        (rotatef (aref (heap heap) root-index)
                 (aref (heap heap) index))
        (heapify-down heap root-index)))))



