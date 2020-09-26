(defclass uf-tree ()
  ((parents
    :initarg :parents
    :accessor parents)
   (group-count
    :initarg :group-count
    :accessor group-count)))


(defun uf-create (size)
  (declare (fixnum size))
  (make-instance 'uf-tree
                 :parents (make-array size :initial-element -1)
                 :group-count size))

(defmethod uf-find ((uf uf-tree) (x fixnum))
  (if (minusp (aref (parents uf) x))
      x
      (setf (aref (parents uf) x)
            (uf-find (aref (parents uf) x)))))

(defmethod uf-show-parents ((uf uf-tree))
  (map 'vector
       (lambda (x)
         (if (minusp x)
             x
             (uf-find uf x)))
       (parents uf)))
  

(defmethod uf-unite ((uf uf-tree) (x fixnum) (y fixnum))
  (when (> x y)
    (rotatef x y))
  (let ((x-parent (uf-find uf x))
        (y-parent (uf-find uf y)))
    (unless (= x-parent y-parent)
      (incf (aref (parents uf) x-parent)
            (aref (parents uf) y-parent))
      (setf (aref (parents uf) y-parent) x-parent)
      (decf (group-count uf)))))

(defmethod uf-get-tree-size ((uf uf-tree) (x fixnum))
  (- (aref (parents uf) (uf-find uf x))))


(defmethod uf-count-trees ((uf uf-tree))
  (group-count uf))


(defmethod uf-friends-p ((uf uf-tree) (x fixnum) (y fixnum))
  (= (uf-find uf x)
     (uf-find uf y)))
