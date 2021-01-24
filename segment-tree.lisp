;;;
;;; BOF
;;;

;; Segment-tree (1-indexed)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symb (&rest args)
    (intern
     (with-output-to-string (s)
       (dolist (x args)
         (princ (string-upcase x) s))))))

(defmacro define-segment-tree (struct-name &key element-type result-type fn e)
  `(progn
     
     (defstruct (,struct-name (:conc-name ,(symb (symbol-name struct-name) "-"))
                              (:constructor ,(symb "%make-" (symbol-name struct-name))))
       (m nil :type fixnum)
       (data nil :type (simple-array ,element-type (*))))
     
     (defun ,(symb "make-" (symbol-name struct-name)) (size)
       (declare (fixnum size))
       (let ((m (sb-int:named-let rec ((m 1))
                  (if (>= m size)
                      m
                      (rec (ash m 1))))))
         (declare (fixnum m))
         (,(symb "%make-" (symbol-name struct-name)) :m m
                                                     :data (make-array (the fixnum (ash m 1))
                                                                       :element-type ',element-type
                                                                       :adjustable nil
                                                                       :initial-element ,e))))

     (defun ,(symb (symbol-name struct-name) "-get-val") (seg l r)
       (declare (,struct-name seg)
                (fixnum l r))
       (with-slots (m data) seg
         (let ((l (+ l (,(symb (symbol-name struct-name) "-m") seg)))
               (r (+ r (,(symb (symbol-name struct-name) "-m") seg))))
           (declare (fixnum l r))
           (loop while (< l r)
                 with res of-type ,result-type = ,e
                 when (logbitp 0 l)
                   do (setf res (,fn res (aref data l)))
                   and do (incf l)
                 when (logbitp 0 r)
                   do (setf res (,fn res (aref data (1- r))))
                   and do (decf r)
                 do (setq l (ash l -1))
                    (setq r (ash r -1))
                 finally
                    (return res)))))

     (defun ,(symb (symbol-name struct-name) "-update") (seg i val)
       (declare (,struct-name seg)
                (fixnum i)
                (,element-type val))
       (with-slots (m data) seg
         (let ((i (the fixnum (+ i m))))
           (declare (fixnum i))
           (setf (aref data i) val)
           (let ((i (ash i -1)))
             (declare (fixnum i))
             (loop while (plusp i)
                   do (setf (aref data i)
                            (the ,result-type
                                 (,fn (aref data (the fixnum (logior 0 (ash i 1))))
                                      (aref data (the fixnum (logior 1 (ash i 1)))))))
                      (setf i (the fixnum (ash i -1))))))))))

;; Modify here depending to circumustances
;; e.g. Range-Minimum-Query(RMQ)
(define-segment-tree seg
  :element-type fixnum
  :result-type fixnum
  :fn min
  :e most-positive-fixnum)

;;;
;;; EOF
;;;
