(defstruct (pivot-tree (:conc-name pt-)
                       (:constructor %make-pt (value pivot depth &key left right)))
  (value 0 :type fixnum)
  (pivot 0 :type fixnum)
  (depth 0 :type fixnum)
  (left nil :type (or null pivot-tree))
  (right nil :type (or null pivot-tree)))

(defun make-pt (depth)
  (%make-pt (ash 1 depth)
            (ash 1 depth)
            depth))


(defun lsb (x)
  (logand x (- x)))

(defun to-left (x)
  (- x (ash (lsb x) -1)))

(defun to-right (x)
  (+ x (ash (lsb x) -1)))

(defun lower-bound (pt value)
  (labels ((%lb (pt value depth)
             (cond
               ((or (> depth ))))))))

(defun insert (pt value)
  (let ((value (1+ value)))
    (cond
      ((= (pt-value pt) value)
       (error "~a is already inserted to tree" value))
      (()))))
