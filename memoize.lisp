;;; TODO


(defclass promise ()
  ((thunk :initarg :thunk)
   (cache :initarg :cache)
   (forced-p :initarg :forced-p)))

(defmacro delay (expr)
  `(make-instance 'promise
                  :thunk (lambda () ,expr)
                  :cache nil
                  :forced-p nil))

(defmethod force ((promise promise))
  (with-slots (thunk cache forced-p) promise
    (unless forced-p
      (setf cache (funcall thunk))
      (setf forced-p t))
    cache))



(defun )
(let ((cache (make-hash-table :test #'equal)))
  (defun (&rest args)
      (multiple-value-bind (val win) (gethash args cache)
        (if win
            val
            (setf (gethash args cache)
                  (apply fn args))))))

(defmacro def-memoized-function (name lambda-list &body body)
  (let ((cache (gensym))
        (val (gensym))
        (win (gensym)))
    `(let ((,cache (make-hash-table :test #'equal)))
       (defun ,name ,lambda-list
         (multiple-value-bind (,val ,win) (gethash (list ,@lambda-list) ,cache)
           (if ,win
               ,val
               (setf (gethash (list ,@lambda-list) ,cache)
                     (progn
                       ,@body))))))))

(def-memoized-function fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))
