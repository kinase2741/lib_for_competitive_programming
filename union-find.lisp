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
                            (:constructor %make-uf))
  (dat nil :type simple-array)
  (size nil :type fixnum)
  (count nil :type fixnum))


(defmethod make-uf-tree ((size fixnum))
  (%make-uf :dat (make-array size :element-type 'fixnum
                                  :adjustable nil
                                  :initial-element -1)
            :size size
            :count size))

(defmacro ref (uf idx)
  `(aref (uf-dat ,uf) ,idx))

(defmethod find ((uf union-find-tree)
                 (x fixnum))
  (if (minusp (ref uf x))
      x
      (setf (ref uf x)
            (find uf (ref uf x)))))


(defmethod show-parents ((uf union-find-tree))
  ;; Return the parent of each member in list.
  (loop for i
          below (uf-size uf)
        collect (if (minusp (ref uf i))
                    i
                    (find uf (ref uf i)))))
  

(defmethod unite ((uf union-find-tree)
                  (x fixnum)
                  (y fixnum))
  (let ((x-parent (find uf x))
        (y-parent (find uf y)))
    (when (> x-parent y-parent)
      (rotatef x-parent y-parent))
    (unless (= x-parent y-parent)
      (incf (ref uf x-parent)
            (ref uf y-parent))
      (setf (ref uf y-parent)
            x-parent)
      (decf (uf-count uf)))))

(defmethod get-tree-size ((uf union-find-tree)
                          (x fixnum))
  (- (ref uf (find uf x))))

(defmethod friends-p ((uf union-find-tree)
                      (x fixnum)
                      (y fixnum))
  (= (find uf x)
     (find uf y)))

(defmethod count-trees ((uf union-find-tree))
  (uf-count uf))

(in-package :cl-user)

;;;
;;; EOF
;;;
