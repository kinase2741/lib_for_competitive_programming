

(defun make-bh (node)
  (list node nil nil))


(defun node (tree)
  (car tree))

(defun l-tree (tree)
  (cadr tree))

(defun r-tree (tree)
  (cddr tree))

(defun l-node (tree)
  (node (l-tree tree)))

(defun r-node (tree)
  (node (r-tree tree)))

(defun bh-insert (tree new-node &optional (test #'<))
  (cond
    ((null tree) (make-bh new-node))
    ((= (node tree) new-node) tree)
    ((funcall test (l-node tree) new-node)
     (list (node tree)
           (bh-insert (l-tree tree) new-node test)
           (r-tree tree)))
    (t
     (list (node tree)
           (l-tree tree)
           (bh-insert (r-tree tree) new-node test)))))
