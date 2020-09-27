;; quoted from https://gist.github.com/myaosato/114c43b4055dfb914b47fed025fca21c#file-binary-heap-lisp

;;;; heap
(defstruct (binary-heap (:conc-name bh-))
  (predicate #'>=)
  (nodes (make-array (list 1024) :adjustable t :fill-pointer 0)))

(defun bh-empty (binary-heap)
  (= (length (bh-nodes binary-heap)) 0))

(defun bh-tail (binary-heap)
  (1- (length (bh-nodes binary-heap))))

(defun bh-invalid-index (binary-heap index)
  (> index (bh-tail binary-heap)))
  
(defun bh-parent (index)
  (when (> index 0)
    (1- (ceiling index 2))))

(defun bh-left (index)
  (+ (* index 2) 1))

(defun bh-right (index)
  (+ (* index 2) 2))

(defun bh-ref (binary-heap index)
  (if (bh-invalid-index binary-heap index)
      nil
      (aref (bh-nodes binary-heap) index)))

(defun bh-ref (binary-heap index)
  (when (<= index (bh-tail binary-heap))
    (aref (bh-nodes binary-heap) index)))

(defsetf bh-ref (binary-heap index) (value)
  `(setf (aref (bh-nodes ,binary-heap) ,index) ,value))

(defun bh-compare (binary-heap p c)
  (cond ((bh-invalid-index binary-heap p)
         nil)
        ((bh-invalid-index binary-heap c)
         t)
        (t
         (funcall (bh-predicate binary-heap) (bh-ref binary-heap p) (bh-ref binary-heap c)))))

(defun bh-rotate (binary-heap a b)
  (rotatef (bh-ref binary-heap a) (bh-ref binary-heap b)))


(defun up-heap (value binary-heap)
  (vector-push-extend value (bh-nodes binary-heap))
  (let* ((c (bh-tail binary-heap))
         (p (bh-parent c)))
    (loop :while p
          :until (bh-compare binary-heap p c)
          :finally (return binary-heap)
          :do (bh-rotate binary-heap p c)
          :do (setf c p)
          :do (setf p (bh-parent c))))
    binary-heap)

(defun down-heap (binary-heap)
  (when (not (bh-empty binary-heap))
    (prog1 (bh-ref binary-heap 0)
      (bh-rotate binary-heap 0 (bh-tail binary-heap))
      (vector-pop (bh-nodes binary-heap))
      (when (not (bh-empty binary-heap))
        (let* ((c 0))
          (loop :for l := (bh-left c)
                :for r := (bh-right c)
                :until (and (bh-compare binary-heap c l) (bh-compare binary-heap c r))
                :do (cond ((bh-compare binary-heap l r)
                           (bh-rotate binary-heap c l)
                           (setf c l))
                          (t
                           (bh-rotate binary-heap c r)
                           (setf c r)))))))))
