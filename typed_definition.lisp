(defmacro defnt (function-spec arg-spec &body body)
  (let ((function-name (first function-spec))
        (function-type (second function-spec))
        (arg-names (mapcar #'first arg-spec))
        (arg-types (mapcar #'second arg-spec)))
    `(declaim (ftype (function ,arg-types ,function-type) ,function-name))
    `(defun ,function-name ,arg-names
       (declare ,@(mapcar (lambda (arg type)
                            (list type arg))
                          arg-names
                          arg-types))
       ,@body)))



(defmacro tlet (bindings &body body)
  "ex. (tlet ((x 1 fixnum) (y 3 fixnum)) (body...))"
  (let ((arg-names (mapcar #'first bindings))
        (arg-inits (mapcar #'second bindings))
        (arg-types (mapcar #'third bindings)))
    `(let (,@(mapcar (lambda (arg init)
                       (list arg init))
                     arg-names
                     arg-inits))
       (declare ,@(mapcar (lambda (arg type)
                            (list type arg))
                          arg-names
                          arg-types))
       ,@body)))



(defmacro tlabels (definitions &body body)
  (macrolet ((sub-def (function-spec arg-spec &body body)
               (let ((function-name (first function-spec))
                     (function-type (second function-spec))
                     (arg-names (mapcar #'first arg-spec))
                     (arg-types (mapcar #'rest arg-spec)))
                 `(labels ((,function-name ,arg-names
                             (declare ,@(mapcar (lambda (name type)
                                                  (list type name))
                                                arg-names
                                                arg-types))
                             ,@body))))))
    (if (null definitions)
        ,@body
        (destructuring-bind (fs as b) (first definitions)
          `((sub-def fs as b)
            ,@body)))))




(tlabels (((add fixnum) ((x fixnum)
                         (y fixnum))
           (+ x y)))
  (add 1 3))
