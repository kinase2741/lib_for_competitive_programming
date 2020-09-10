(defstruct uf-tree
  (size 30)
  (parents (make-array size)))


(defmethod uf-find ((tree uf-tree) (x integer))
  (if (minusp (aref (uf-tree-parents tree) x))
      x
      (setf (aref (uf-tree-parents tree) x)
            (uf-find tree (aref (uf-tree-parents tree) x)))))


(defme)
