;;  BOF

;; Binary heap (1-based)

(defpackage :heap
  (:shadow :count)
  (:use :cl)
  (:export :make-heap :count :empty-p :peek :insert :extract))

(in-package :heap)

(defstruct (binary-heap (:conc-name heap-)
                        (:constructor %make-heap))
  ;; 
  ;; Example: TODO
  ;;
  (dat nil :type vector)
  (predicate nil :type function))

(defun make-heap (size &key (predicate #'<))
  (%make-heap :dat (make-array *default-heap-size*
                   :element-type t
                   :adjustable t
                   :fill-pointer 1)
              :predicate predicate))

(defmethod count ((heap binary-heap))
  (the fixnum (1- (fill-pointer (heap-dat heap)))))

(defmethod empty-p ((heap binary-heap))
  (zerop (count heap)))

(defmethod insert ((heap binary-heap) item)
  (vector-push-extend item (heap-dat heap))
  (heapify-up heap (count heap)))

(defmethod heapify-up ((heap binary-heap)
                       (node-index fixnum))
  (let ((parent-index (floor node-index 2)))
    (labels ((ordered-p (pred parent child)
               (funcall pred
                        (aref (heap-dat heap) parent)
                        (aref (heap-dat heap) child))))
      (when (and (plusp parent-index)
                 (not (ordered-p (heap-predicate heap)
                                 parent-index
                                 node-index)))
        (rotatef (aref (heap-dat heap) node-index)
                 (aref (heap-dat heap) parent-index))
        (heapify-up heap parent-index)))))

(defmethod peek ((heap binary-heap))
  (aref (heap-dat heap) 1))

(defmethod extract ((heap binary-heap))
  (when (empty-p heap)
    (error "Heap is empty."))
  (let ((res (aref (heap-dat heap) 1)))
        (prog1 res
          (setf (aref (heap-dat heap) 1)
                (aref (heap-dat heap)
                      (heap-count heap)))
          (vector-pop (heap-dat heap))
          (heapify-down heap 1))))

(defmethod heapify-down ((heap binary-heap)
                         (root-index fixnum))
  (let ((root root-index)
        (l (* root-index 2))
        (r (1+ (* root-index 2)))
        (pred (heap-predicate heap)))
    (labels ((ordered-p (predicate p c)
               (funcall predicate
                        (aref (heap-dat heap) p)
                        (aref (heap-dat heap) c)))
             (swap! (x y)
               (rotatef (aref (heap-dat heap) x)
                        (aref (heap-dat heap) y))))
      (cond ((and (<= l (count heap))
                  (or (> r (count heap))
                      (ordered-p pred l r))
                  (not (ordered-p pred root l)))
             (swap! root l)
             (heapify-down heap l))
            ((and (<= r (count heap))
                  (not (ordered-p pred root r)))
             (swap! root r)
             (heapify-down heap r))))))


(defparameter *default-heap-size* 1000)

(in-package :cl-user)

;; EOF
