;;;
;;; BOF
;;;

;; substitute for case-macro 

(defmacro match ((keyform &key (test #'equal)) &body cases)
  `(cond
     ,@(mapcar (lambda (case)
                 (case case
                   ('otherwise `(t ,@(rest case)))
                   (otherwise `((funcall ,test ,keyform ,(first case)) ,@(rest case)))))
               cases)))

;;;
;;; BOF
;;;
