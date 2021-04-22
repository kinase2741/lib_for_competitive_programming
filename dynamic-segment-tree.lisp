;;;
;;; Dynamic segment tree (0-indexed)
;;;



#|
TODO:
- テンプレートもどきで関数を埋め込みたい(defmethodは木構造に対して使いづらい)
- element-type
- result-type
- identity
- fn
- max(格納出来る値のmax)
|#


(defmacro define-dynamic-segment-tree (name &key op op-identity max-value element-type result-type)
  (flet ((symb (&rest args)
           (intern
            (with-output-to-string (s)
              (mapc (lambda (arg)
                      (princ (string-upcase arg) s))
                    args)))))
    (let* ((range-end (sb-int:named-let rec ((n0 1))
                        (if (>= n0 max-value)
                            n0
                            (rec (ash n0 1)))))
           (name-str (symbol-name name))
           (%make-dseg (symb "%make-" name-str))
           (conc-name (symb name-str "-"))
           (%fold (symb "%" name-str "-fold"))
           (fold (symb name-str "-fold"))
           (%update (symb "%" name-str "-update"))
           (update (symb name-str "-update"))
           (update! (symb name-str "-update!")))
      `(progn
         (defstruct (,name (:conc-name ,conc-name)
                           (:constructor ,%make-dseg (&key (val ,op-identity) l r)))
           (val val :type ,element-type)
           (l nil :type (or null ,name))
           (r nil :type (or null ,name)))

         (defun ,fold (dseg l r)
           (,%fold dseg l r 0 ,range-end))

         (defun ,%fold (dseg l r begin end)
           (cond
             ((null dseg) ,op-identity)
             ((and (<= l begin)
                   (<= end r))
              (dseg-val dseg))
             ((or (<= r begin)
                  (<= end l))
              ,op-identity)
             (:else
              (let ((mid (ash (+ begin end) -1)))
                (,op (if (null (dseg-l dseg))
                         ,op-identity
                         (,%fold (dseg-l dseg)
                                 l
                                 r
                                 begin
                                 mid))
                     (if (null (dseg-r dseg))
                         ,op-identity
                         (,%fold (dseg-r dseg)
                                 l
                                 r
                                 mid
                                 end)))))))

         (defun ,update (dseg index value)
           (,%update dseg
                     index
                     value
                     0
                     ,range-end))

         (defmacro ,update! (dseg index value)
           (multiple-value-bind (args argvs val setter getter)
               (get-setf-expansion dseg)
             (let ((update ',update))
               `(let ,(mapcar #'list args argvs)
                  (let ((,@val (,update ,getter ,index ,value)))
                    ,setter)))))



         (defun ,%update (dseg index value begin end)
           (when (> (abs (- begin end))
                    1)
             (let ((mid (ash (+ begin end) -1)))
               (if (< index mid)
                   (,%make-dseg :val (if (null dseg)
                                         value
                                         (,op value (dseg-val dseg)))
                                :l (,%update (when dseg (dseg-l dseg))
                                             index
                                             value
                                             begin
                                             mid)
                                :r (when dseg (dseg-r dseg)))
                   (,%make-dseg :val (if (null dseg)
                                         value
                                         (,op value (dseg-val dseg)))
                                :l (when dseg (dseg-l dseg))
                                :r (,%update (when dseg (dseg-r dseg))
                                             index
                                             value
                                             mid
                                             end))))))))))

(define-dynamic-segment-tree dseg
  :op (lambda (x y) (+ x y))
  :op-identity 0
  :max-value #.(1- (ash 1 32))
  :element-type fixnum
  :result-type fixnum)
