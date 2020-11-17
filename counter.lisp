(defun gen-counter (sequence &key (test 'eql) (size 10000))
  (let ((ht (make-hash-table :test test :size size)))
    (map nil
         (lambda (x)
           (incf (gethash x ht 0)))
         sequence)
    ht))

(defun counter-items (counter)
  (loop for key being each hash-key of counter
          using (hash-value val)
        collect (cons key val)))

(defun counter-keys (counter)
  (loop for key being each hash-key of counter
        collect key))

(defun counter-values (counter)
  (loop for val being each hash-value of counter
        collect val))

(defun counter-size (counter)
  (length (counter-keys counter)))

