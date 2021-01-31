;;;
;;; BOF
;;;

;; Union-Find Tree (0-indexed)

(defpackage :union-find
  (:nicknames :uf)
  (:use :cl)
  (:shadow :find)
  (:export :make-uf-tree :find :unite :friends-p :show-parents :get-tree-size :count-trees))

(in-package :union-find)

(defstruct (union-find-tree (:conc-name uf-)
                            (:constructor make-uf-tree (size)))
  (dat (make-array size :element-type 'fixnum
                        :adjustable nil
                        :initial-element -1)
   :type simple-array)
  (size size :type fixnum)
  (count size :type fixnum))


(defun find (uf x)
  (declare (union-find-tree uf)
           (fixnum x))
  (with-slots (dat size count) uf
    (declare (ignorable dat size count))
    (if (minusp (aref dat x))
        x
        (setf (aref dat x)
              (find uf (aref dat x))))))


(defun show-parents (uf)
  ;; Return the parent of each member in list.
  #-swank (declare (ignore uf))
  #+swank
  (locally (declare (union-find-tree uf))
    (with-slots (dat size count) uf
      (declare (ignorable count))
      (loop for i
              below size
            collect (if (minusp (aref dat i))
                        i
                        (find uf (aref dat i)))))))
  

(defmethod unite (uf x y)
  (declare (union-find-tree uf)
           (fixnum x y))
  (with-slots (dat size count) uf
    (declare (ignore size))
    (let ((x-parent (find uf x))
          (y-parent (find uf y)))
      (declare (fixnum x-parent y-parent))
      (when (> x-parent y-parent)
        (rotatef x-parent y-parent))
      (unless (= x-parent y-parent)
        (incf (aref dat x-parent)
              (aref dat y-parent))
        (setf (aref dat y-parent)
              x-parent)
        (decf count)))))

(defun get-tree-size (uf x)
  (declare (union-find-tree uf)
           (fixnum x))
  (with-slots (dat size count) uf
    (declare (ignorable size count))
    (the fixnum (- (aref dat (find uf x))))))

(defun friends-p (uf x y)
  (declare (union-find-tree uf)
           (fixnum x y))
  (= (find uf x)
     (find uf y)))

(defun count-trees (uf)
  (declare (union-find-tree uf))
  (uf-count uf))

(in-package :cl-user)

;;;
;;; EOF
;;;
