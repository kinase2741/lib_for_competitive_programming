;;;
;;; BOF
;;;

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
  (pred nil :type function)
  (key nil :type function))

(defun make-heap (size &key (predicate #'<) (key #'identity))
  (%make-heap :dat (make-array size
                   :element-type t
                   :adjustable t
                   :fill-pointer 1)
              :pred predicate
              :key key))

(defmethod count ((heap binary-heap))
  (the fixnum (1- (fill-pointer (heap-dat heap)))))

(defmethod empty-p ((heap binary-heap))
  (zerop (count heap)))

(defmethod insert ((heap binary-heap) item)
  (vector-push-extend item (heap-dat heap))
  (heapify-up heap (count heap)))


(defmethod heapify-up ((heap binary-heap)
                       (node-index fixnum))
  (with-slots (dat pred key) heap
    (let ((parent-index (floor node-index 2)))
      (labels ((ordered-p (pred parent child)
                 (funcall pred
                          (funcall key (aref dat parent))
                          (funcall key (aref dat child)))))
        (when (and (plusp parent-index)
                   (not (ordered-p pred
                                   parent-index
                                   node-index)))
          (rotatef (aref dat node-index)
                   (aref dat parent-index))
          (heapify-up heap parent-index))))))

(defmethod peek ((heap binary-heap))
  (aref (heap-dat heap) 1))

(defmethod extract ((heap binary-heap))
  (with-slots (dat pred key) heap
    (when (empty-p heap)
      (error "Heap is empty."))
    (let ((res (aref dat 1)))
      (prog1 res
        (setf (aref dat 1)
              (aref dat
                    (count heap)))
        (vector-pop dat)
        (heapify-down heap 1)))))

(defmethod heapify-down ((heap binary-heap)
                         (root-index fixnum))
  (with-slots (dat pred key) heap
    (let ((root root-index)
          (l (* root-index 2))
          (r (1+ (* root-index 2))))
      (labels ((ordered-p (pred p c)
                 (funcall pred
                          (funcall key (aref dat p))
                          (funcall key (aref dat c))))
               (swap! (x y)
                 (rotatef (aref dat x)
                          (aref dat y))))
        (cond ((and (<= l (count heap))
                    (or (> r (count heap))
                        (ordered-p pred l r))
                    (not (ordered-p pred root l)))
               (swap! root l)
               (heapify-down heap l))
              ((and (<= r (count heap))
                    (not (ordered-p pred root r)))
               (swap! root r)
               (heapify-down heap r)))))))


(defparameter *default-heap-size* 1000)

(in-package :cl-user)

;;;
;;; EOFs
;;;
