(defstruct (counter (:constructor %make-counter))
  (hash nil :type hash-table))

(defun make-counter (sequence &key (test #'eql) (size 10000))
  (let ((counter (%make-counter :hash (make-hash-table :test test :size size))))
    (map nil
         (lambda (x)
           (incf (gethash x (counter-hash counter) 0)))
         sequence)
    counter))

(defmacro counter-ref (counter key)
  `(gethash ,key ,(counter-hash counter) 0))

(defmacro counter-inc (counter key)
  `(incf (counter-ref ,counter ,key)))

(defmethod counter-items ((counter counter))
  (loop for key being each hash-key of (counter-hash counter)
          using (hash-value val)
        collect (cons key val)))

(defmethod counter-keys ((counter counter))
  (loop for key being each hash-key of (counter-hash counter)
        collect key))

(defmethod counter-values ((counter counter))
  (loop for val being each hash-value of (counter-hash counter)
        collect val))

(defmethod counter-size ((counter counter)) 
  (hash-table-count (counter-hash counter)))

