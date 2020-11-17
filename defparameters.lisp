(defmacro defparameters (forms)
  `(progn
     ,@(mapcar (lambda (form)
                 `(defparameter ,(first form) ,(second form)))
               forms)))


(defparameters
    ((dog 1)
     (cat 2)
     (mouse 3)))

