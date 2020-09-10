; TODO

(let (parents)
  ;; "parents" retain uf-tree

  (defun uf-init (size)
    (setf parents (make-array size :initial-element -1)))

  (defun uf-show-parents ()
    parents)
  
  (defun uf-find (x)
    (if (minusp (aref parents x))
        x ; x is root
        (setf (aref parents x) (uf-find (aref parents x)))))

  (defun uf-unite (x y)
    (when (> x y)
      (rotatef x y))
    (let ((x-parent (uf-find x))
          (y-parent (uf-find y)))
      (unless (= x-parent y-parent)
        (incf (aref parents x-parent)
              (aref parents y-parent))
        (setf (aref parents y-parent) x-parent))))

  (defun uf-get-tree-size (x)
    (- (aref parents (uf-find x))))

  (defun uf-conut-trees ()
    (length (remove-duplicates
             (mapcar (lambda (idx)
                       (uf-find idx))
                     (loop for i below (length parents) collect i))
             :test #'=)))

  (defun uf-friends-p (x y)
    (= (uf-find x)
       (uf-find y))))
