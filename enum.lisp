(defun group-by (fn list &key key)
  (let ((memo (make-hash-table :test #'equal)))
    (mapc (lambda (x)
            (let ((x (funcall (or key 'identity) x)))
              (push x (gethash (funcall fn x) memo))))
          list)
    (let ((res nil))
      (maphash (lambda (_ val)
                 (push (sort val #'<) res))
               memo)
      res)))

(defun chunk-every (count list &optional (step count))
  (let ((acc nil))
    (labels ((sub (xs)
               (cond
                 ((null (nthcdr (1- count) xs))
                  (when xs
                    (push xs acc)))
                 (t (push (butlast xs (- (length xs)
                                         count))
                          acc)
                    (sub (nthcdr step xs))))))
      (sub list)
      (reverse acc))))

(defun map-every (fn list &optional (count 2) (step count))
  (mapcar (lambda (xs) (apply fn xs)) (chunk-every count list step)))
