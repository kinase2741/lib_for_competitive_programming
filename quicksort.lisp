;;;
;;; BOF
;;;

;; Sorting Network and Quicksort
;; Reference:
;;   Doug Hoyte - Let Over Lambda Edition 1.0 (Lulu.com; April 2, 2008)
;;   http://staryoshi.hatenablog.com/entry/2016/01/27/200157

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symb (&rest args)
    (intern
     (with-output-to-string (s)
       (dolist (x args)
         (princ (string-upcase x) s))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (defun build-butcher-sn (n)
      (let* (network
             (tee (ceiling (log n 2)))
             (p (ash 1 (- tee 1))))
        (loop while (> p 0)
              do (let ((q (ash 1 (- tee 1)))
                       (r 0)
                       (d p))
                   (loop while (> d 0)
                         do (loop for i from 0 to (- n d 1)
                                  do (when (= (logand i p) r)
                                       (push (cons i (+ i d)) network)))
                            (setf d (- q p)
                                  q (ash q -1)
                                  r p)))
                 (setf p (ash p -1)))
        (nreverse network)))))

(defstruct %pair
  (sup nil :type fixnum)
  (pos nil :type fixnum))

(defmacro %proc (arr element-type l r complement threshold)
  `(ecase (the fixnum (- ,r ,l))
     ,@(loop for len from 1 to threshold collect
             `(,len (progn
                      ,@(mapcar (lambda (pair)
                                  `(let ((a #1=(aref ,arr (the fixnum (+ l ,(car pair)))))
                                         (b #2=(aref ,arr (the fixnum (+ l ,(cdr pair))))))
                                     (declare (,element-type a b))
                                     (if (,complement a b)
                                         (setf #1# b
                                               #2# a))))
                                (build-butcher-sn (1+ len))))))))

(defmacro define-quick-sort (&key suffix complement element-type threshold)
  
  `(progn
     (declaim (inline ,(symb "sn-sort" suffix)))
     (defun ,(symb "sn-sort" suffix) (array l r)
       (declare (optimize (speed 3) (safety 0) (debug 0))
                ((simple-array ,element-type (*)) array)
                (fixnum l r)
                (inline ,complement))
       (%proc array ,element-type l r ,complement ,threshold))

     (declaim (inline ,(symb "quick-sort" suffix)))
     (defun ,(symb "quick-sort" suffix) (array &optional (begin 0) (end (length array)))
       "Destructivery sort an array. Always returns NIL."
       ;; Reference: http://staryoshi.hatenablog.com/entry/2016/01/27/200157
       (declare (optimize (speed 3) (safety 0) (debug 0))
                ((simple-array ,element-type (*)) array)
                (fixnum begin end)
                (inline ,complement))
       (macrolet ((while (test &body body)
                    `(loop while ,test do ,@body)))
         (let ((stack (list (make-%pair :sup begin
                                        :pos (the fixnum (1- end))))))
           (declare (list stack))
           (loop while stack
                 for pair of-type %pair = (pop stack)
                 for l of-type fixnum = (%pair-sup pair)
                 for r of-type fixnum = (%pair-pos pair)
                 if (<= (abs (the fixnum (- l r))) ,threshold)
                   do (,(symb "sn-sort" suffix) array l r)
                 else
                   do (let ((i l)
                            (j r)
                            (pivot (aref array (the fixnum (ash (the fixnum (+ l r)) -1)))))
                        (declare (fixnum i j)
                                 (,element-type pivot))
                        (while (<= i j)
                               (while (,complement (aref array i) pivot)
                                      (incf i))
                               (while (,complement (aref array j) pivot)
                                      (decf j))
                               (when (<= i j)
                                 (rotatef (aref array i) (aref array j))
                                 (incf i)
                                 (decf j)))
                        (when (< l j)
                          (push (make-%pair :sup l
                                            :pos j)
                                stack))
                        (when (< i r)
                          (push (make-%pair :sup i
                                            :pos r)
                                stack)))))))))

;; Usage : (quicksort-less array) ;=> NIL

(define-quick-sort
    :suffix "-less"
  :complement >
  :element-type fixnum
  :threshold 15)

;; (define-quick-sort
;;     :suffix "-greater"
;;   :complement <
;;   :element-type fixnum
;;   :threshold 15)


;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (declaim (inline list>))
;;   (defun list> (xs ys)
;;     (declare (list xs ys))
;;     (if (= (the fixnum (first xs))
;;            (the fixnum (first ys)))
;;         (> (the fixnum (second xs))
;;            (the fixnum (second ys)))
;;         (> (the fixnum (first xs))
;;            (the fixnum (first ys))))))

;; (define-quick-sort
;;     :suffix "-list-less"
;;   :complement list>
;;   :element-type list
;;   :threshold 15)

;;;
;;; EOF
;;;
