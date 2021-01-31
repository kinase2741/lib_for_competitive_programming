;;;
;;; BOF
;;;

;; Binary-indexed-tree (1-indexed)
;; Reference: http://hos.ac/slides/20140319_bit.pdf

(defmacro %defun (fn-spec args-spec &body body)
  `(progn
     (declaim (ftype (function (,@(mapcar #'second args-spec)) ,(second fn-spec)) ,(first fn-spec)))
     (defun ,(first fn-spec) (,@(mapcar #'first args-spec))
       (declare ,@(mapcar (lambda (args)
                            `(type ,(second args) ,(first args)))
                          args-spec))
       ,@body)))

(defmacro define-bit (name &key nickname element-type result-type)
  (flet ((symb (&rest args)
           (intern
            (with-output-to-string (s)
              (let ((*standard-output* s))
                (mapc #'princ (mapcar #'string-upcase args)))))))
    (let* ((nickname-str (symbol-name nickname))
           (conc-name (symb nickname-str "-"))
           (constructor (symb "make-" nickname-str))
           (add (symb nickname-str "-add"))
           (fold (symb nickname-str "-fold"))
           (fold-range (symb nickname-str "-fold-range"))
           (lower-bound (symb nickname-str "-lower-bound")))
      `(progn
         (defstruct (,name (:conc-name ,conc-name)
                           (:constructor ,constructor (size)))
           (size size :type fixnum)
           (data (make-array (1+ size)
                             :element-type ',element-type
                             :adjustable nil
                             :initial-element 0)
            :type (simple-array ,element-type (*))))

         (declaim (inline ,add ,fold ,fold-range ,lower-bound))
         (%defun (,add null) ((bit ,name)
                              (index fixnum)
                              (val fixnum))
           (with-slots (size data) bit
             (assert (<= 1 index size))
             (loop while (<= index size)
                   do (incf (aref data index) val)
                      (incf index (logand index (- index))))))

         (%defun (,fold ,result-type) ((bit ,name)
                                       (index fixnum))
           "get sum of value in [1,index)"
           (with-slots (size data) bit
             (loop while (plusp index)
                   with res of-type ,result-type = 0
                   do (incf res (aref data index))
                      (decf index (logand index (- index)))
                   finally (return res))))

         (%defun (,fold-range ,result-type) ((bit ,name)
                                             (l fixnum)
                                             (r fixnum))
           (the ,result-type
                (- (,fold bit (1- r))
                   (,fold bit (1- l)))))
         
         (%defun (,lower-bound ,result-type) ((bit ,name)
                                              (w ,result-type))
           "a_1 + a_2 + ... + a_i >= wとなる最小のiを返す"
           (with-slots (size data) bit
             (the ,result-type
                  (if (<= w 0)
                      0
                      (labels ((%get-range (r)
                                 (declare (fixnum r))
                                 (the fixnum
                                      (if (> r size)
                                          (ash r -1)
                                          (%get-range (ash r 1)))))
                               (%lower-bound (range w acc)
                                 (declare (fixnum range w acc))
                                 (the fixnum
                                      (cond ((<= range 0) acc)
                                            ((and (<= (+ range acc)
                                                      size)
                                                  (< (aref data (+ range acc))
                                                     w))
                                             (%lower-bound (ash range -1)
                                                           (- w (aref data (+ range acc)))
                                                           (+ acc range)))
                                            (t
                                             (%lower-bound (ash range -1)
                                                           w
                                                           acc))))))
                        (1+ (%lower-bound (%get-range 1)
                                          w
                                          0)))))))))))

(define-bit binary-indexed-tree
  :nickname bit
  :element-type fixnum
  :result-type fixnum)

;;;
;;; EOF
;;;
