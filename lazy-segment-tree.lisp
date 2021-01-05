;; Lazy-segment-tree (0-indexed)
;; Source: https://tsutaj.hatenablog.com/entry/2017/03/30/224339

(defstruct (lazy-segment-tree (:conc-name lseg-))
  (fn nil :type function)
  (e nil)
  (n nil :type fixnum)
  (node nil :type simple-array)
  (lazy nil :type simple-array))

(defun gen-lazy-seg-tree (fn e vector)
  (let ((size (length vector)))
    (let ((n (sb-int:named-let rec ((n 1))
               (if (>= n size)
                   n
                   (rec (* n 2))))))
      (let ((lseg (make-lazy-segment-tree :fn fn
                                          :e e
                                          :n n
                                          :node (make-array (1- (* n 2))
                                                            :element-type (array-element-type vector)
                                                            :adjustable nil
                                                            :initial-element e)
                                          :lazy (make-array (1- (* n 2))
                                                            :element-type (array-element-type vector)
                                                            :adjustable nil
                                                            :initial-element e))))
        (loop for i of-type fixnum below size do
          (setf (aref (lseg-node lseg) (+ i n -1))
                (aref vector i)))
        (loop for i of-type fixnum
              from (- n 2)
              downto 0
              do (setf (aref (lseg-node lseg) i)
                       (funcall fn
                                (aref (lseg-node lseg) (+ (* i 2) 1))
                                (aref (lseg-node lseg) (+ (* i 2) 2)))))
        lseg))))

(defun lseg-eval ((k fixnum)
                 (l fixnum)
                 (r fixnum)
                 (lseg lazy-segment-tree))
  (when (/= (aref (lseg-lazy lseg)
                  k)
            (lseg-e lseg))
    (incf (aref (lseg-node lseg)
                k)
          (aref (lseg-lazy lseg)
                k)))
  (when (> (- r l) 1)
    (incf )))

(defmethod seg-update ((idx fixnum)
                       val
                       (seg segment-tree))
  (let ((idx (+ (seg-n seg)
                idx
                -1)))
    (setf (aref (seg-node seg) idx)
          val)
    (sb-int:named-let rec ((idx (floor (1- idx) 2)))
      (when (>= idx 0)
        (setf (aref (seg-node seg) idx)
              (funcall (seg-fn seg)
                       (aref (seg-node seg) (+ (* idx 2) 1))
                       (aref (seg-node seg) (+ (* idx 2) 2))))
        (rec (floor (1- idx) 2))))))

(defmethod seg-get-val ((req-l fixnum)
                        (req-r fixnum)
                        (seg segment-tree))
  (labels ((sub (l r k)
             (declare (fixnum l r k))
             (cond ((or (<= r req-l)
                        (<= req-r l))
                    (seg-e seg))
                   ((and (<= req-l l)
                         (<= r req-r))
                    (aref (seg-node seg) k))
                   (t (funcall (seg-fn seg)
                               (sub l
                                    (the fixnum (floor (+ l r)
                                                       2))
                                    (the fixnum (+ (* k 2) 1)))
                               (sub (the fixnum (floor (+ l r)
                                                       2))
                                    r
                                    (the fixnum (+ (* k 2) 2))))))))
    (sub 0 (seg-n seg) 0)))

