;;;
;;; BOF
;;;

;; Skew Heap
;; Reference: http://hos.ac/blog/#blog0001

(defstruct (heap (:constructor make-heap (val &key l r)))
  (val 0 :type list)
  (l nil :type (or null heap))
  (r nil :type (or null heap)))

(defun heap-empty-p (heap)
  (null heap))

(defun heap-meld (l r)
  (declare ((or null heap) l r))
  (the (or null heap)
       (cond
         ((null l) r)
         ((null r) l)
         (:else (when (> (heap-val l) (heap-val r))
                  (rotatef l r))
                (setf (heap-r l) (heap-meld (heap-r l) r))
                (rotatef (heap-l l) (heap-r l))
                l))))

(defmacro heap-push! (heap item)
  (multiple-value-bind (args argvs val setter getter)
      (get-setf-expansion heap)
    `(let ,(mapcar #'list args argvs)
       (let ((,@val (heap-meld (make-heap ,item) ,getter)))
         ,setter))))

(defmacro heap-pop! (heap)
  (let ((item (gensym)))
    (multiple-value-bind (args argvs val setter getter)
        (get-setf-expansion heap)
      `(let ,(mapcar #'list args argvs)
         (let ((,item (heap-val ,getter))
               (,@val (heap-meld (heap-l ,getter)
                                 (heap-r ,getter))))
           ,setter
           ,item)))))

;;;
;;; EOF
;;;
