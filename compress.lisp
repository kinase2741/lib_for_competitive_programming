;; BOF

(declaim (ftype (function (sequence) (values hash-table hash-table)) gen-decompressor))
(defun gen-compressor (sequence &optional (index-origin 0))
  (declare (inline sort sb-impl::stable-sort))
  (flet ((%remove-duplicates-into-list (sequence)
           (let ((used (make-hash-table :test #'eql))
                 (acc nil))
             (declare (hash-table used)
                      (list acc))
             (map nil
                  (lambda (x)
                    (unless (gethash x used)
                      (setf (gethash x used) t)
                      (push x acc)))
                  sequence)
             acc)))
    (let ((set (sort (%remove-duplicates-into-list sequence) #'<))
          (comp (make-hash-table :test #'eql))
          (decomp (make-hash-table :test #'eql)))
      (let ((n (the fixnum (length set))))
        (loop for i of-type fixnum from index-origin below (+ n index-origin)
              for x of-type integer in set
              do (setf (gethash x comp) i)
                 (setf (gethash i decomp) x))
        (values comp decomp)))))

;; EOF
