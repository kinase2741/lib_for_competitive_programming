;;; count lcs


(defun lcs (str1 str2)
  (declare (string str1 str2))
  (let* ((len1 (length str1))
         (len2 (length str2))
         (memo (make-array (* len1 len2)
                           :element-type 'fixnum
                           :adjustable nil
                           :initial-element -1)))
    (declare (fixnum len1 len2)
             ((array fixnum 1) memo))
    (labels ((%encode (x y)
               (declare (fixnum x y))
               (+ x
                  (* len1
                     y)))
             (%dp (i j)
               (declare (fixnum i j))
               (cond
                 ((or (zerop i) (zerop j)) 0)
                 ((/= (aref memo (%encode (1- i)
                                          (1- j)))
                      -1)
                  (aref memo (%encode (1- i) (1- j))))
                 (t
                  (setf (aref memo (%encode (1- i)
                                            (1- j)))
                        (if (char-equal (char str1 (1- i))
                                        (char str2 (1- j)))
                            (1+ (%dp (1- i) (1- j)))
                            (max (%dp (1- i) j)
                                 (%dp i (1- j)))))))))
      (%dp len1 len2))))
