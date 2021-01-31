;;;
;;; BOF
;;;

;; Threading macros

(defmacro -> (init &rest forms)
  `(block nil
     ,(reduce (lambda (xs ys)
                (cond
                  ((atom ys) `(,ys ,xs))
                  ((eq 'as-> (first ys)) `(let ((,(second ys) ,xs))
                                            ,@(nthcdr 2 ys)))
                  ((find :@ ys) (subst xs :@ ys))
                  (t `(,(first ys) ,xs ,@(rest ys)))))
              forms :initial-value init)))

(defmacro ->> (init &rest forms)
  `(block nil
     ,(reduce (lambda (xs ys)
                (cond
                  ((atom ys) `(,ys ,xs))
                  ((find :@ ys) (subst xs :@ ys))
                  (t `(,@ys ,xs))))
              forms :initial-value init)))

(defmacro some-> (init &rest forms)
  `(block nil
     ,(reduce (lambda (xs ys)
                (let ((zs (gensym)))
                  `(let ((,zs ,xs))
                     (when ,zs
                       ,(cond
                          ((atom ys) `(,ys ,zs))
                          ((eq 'as-> (first ys)) `(let ((,(second ys) ,zs))
                                                    ,@(nthcdr 2 ys)))
                          ((find :@ ys) (subst zs :@ ys))
                          (t `(,(first ys) ,zs ,@(rest ys))))))))
              forms :initial-value init)))

(defmacro some->> (init &rest forms)
  `(block nil
     ,(reduce (lambda (xs ys)
                (let ((zs (gensym)))
                  `(let ((,zs ,xs))
                     (when ,zs
                       ,(cond
                          ((atom ys) `(,ys ,zs))
                          ((find :@ ys) (subst zs :@ ys))
                          (t `(,@ys ,zs)))))))
              forms :initial-value init)))

;;;
;;; EOF
;;;
