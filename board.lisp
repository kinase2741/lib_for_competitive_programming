(defun read-characters-to-board (row-size column-size)
  (let ((board (make-array `(,row-size ,column-size)
                           :element-type '(unsigned-byte 4)
                           :adjustable nil)))
    (dotimes (r row-size board)
      (let ((tmp (read-line)))
        (dotimes (c column-size)
          (let ((val (mark->int (char tmp c))))
            (declare ((or (unsigned-byte 4) null) val))
            (if val
                (setf (aref board r c) val)
                (error "Character ~a not found in the alist." (char tmp c)))))))))
