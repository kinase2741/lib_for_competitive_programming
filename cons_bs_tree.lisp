
(defun node (tree)
  (caar tree))

(defun left (tree)
  ( tree))

(defun right (tree)
  (caddr tree))

(defun leaf-p (tree)
  (and (null (left tree))
       (null (right tree))))

(defun count-leaves (tree)
  (1+ (if (leaf-p tree)
          0
          (+ (count-leaves (left tree))
             (count-leaves (right tree))))))

(defun get-tree-depth (tree)
  (if (leaf-p tree)
      1
      (1+ (get-tree-depth (left tree)))))

(defun join-tree (node l-tree r-tree)
  (cons node (cons l-tree r-tree)))

(defun rotate-tree (tree)
  (join-tree node r-tree l-tree))

;;; TODO
(defun insert (tree)
  (let ((d (get-tree-depth tree)))
    (labels ((sub (tree cnt))))))
