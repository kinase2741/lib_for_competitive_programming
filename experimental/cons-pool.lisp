(defvar *pool-size* 200000)
(defvar *cons-pool* (make-array *pool-size* :element-type 'list :initial-element nil))
(defvar *counter* 0)

(defun %initialize ()
  (setf *counter* 0)
  (loop for i below *pool-size*
        do (setf (aref *cons-pool* i) (cons nil nil))))

(defun %cons (se1 se2)
  (prog2
      (psetf (car #1=(aref *cons-pool* *counter*)) se1
             (cdr #1#) se2)
      (aref *cons-pool* *counter*)
    (incf *counter*)))

(defun %free (slot)
  (declare (ignore slot))
  (decf *counter*))

()
