(defmacro define-typed-fn (function-spec (&rest arg-specs) &body body)
  "Quoted from: https://masatoi.github.io/2017/11/21/typed-defun"
  `(progn
     (declaim (ftype (function ,(mapcar #'cadr arg-specs) ,(cadr function-spec)) ,(car function-spec)))
     (defun ,(car function-spec) ,(mapcar #'car arg-specs)
       (declare (optimize (speed 3) (safety 0) (debug 0))
                ,@(mapcar (lambda (arg arg-type)
                            (list 'type arg-type arg))
                          (mapcar #'car arg-specs)
                          (mapcar #'cadr arg-specs)))
       ,@body)))

(defmacro tlet (bindings &body body)
  "Quoted from: https://masatoi.github.io/2017/11/21/typed-defun"
  `(let (,@(mapcar (lambda (binding)
                     (subseq binding 0 2))
                   bindings))
     (declare ,@(mapcar (lambda (binding)
                          (list 'type (caddr binding) (car binding)))
                        bindings))
     ,@body))
