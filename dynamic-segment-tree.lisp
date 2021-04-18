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

(defparameter op-identity 0)
(defparameter *fn* (lambda (x y) (+ x y)))
(defparameter *max* (1- (ash 1 32)))
(defparameter *range-end* (sb-int:named-let rec ((n0 1))
                            (if (>= n0 *max*)
                                n0
                                (rec (ash n0 1)))))


(defstruct (dseg-node (:conc-name dseg-)
                      (:constructor %make-dseg (&key (val op-identity) l r)))
  (val val)
  (l nil)
  (r nil))

(defun dump (dseg)
  (let ((res nil))
    (sb-int:named-let rec ((dseg dseg)
                           (begin 0)
                           (end *range-end*))
      (when (dseg-l dseg)
        (rec (dseg-l dseg)
             begin
             (ash (+ begin end) -1)))
      (when (<= (abs (- begin end))
                1)
        (push (cons begin
                    (dseg-val dseg))
              res))
      (when (dseg-r dseg)
        (rec (dseg-r dseg)
             (ash (+ begin end) -1)
             end)))
    (reverse res)))

(defun fold (dseg l r)
  (%fold dseg l r 0 *range-end*))

(defun %fold (dseg l r begin end)
  (cond
    ((null dseg) op-identity)
    ((and (<= l begin)
          (<= end r))
     (dseg-val dseg))
    ((or (<= r begin)
         (<= end l))
     op-identity)
    (:else
     (let ((mid (ash (+ begin end) -1)))
       (funcall *fn*
                (if (null (dseg-l dseg))
                    op-identity
                    (%fold (dseg-l dseg)
                           l
                           r
                           begin
                           mid))
                (if (null (dseg-r dseg))
                    op-identity
                    (%fold (dseg-r dseg)
                           l
                           r
                           mid
                           end)))))))

(defmacro update! (dseg index value)
  (multiple-value-bind (args argvs val setter getter)
      (get-setf-expansion dseg)
    `(let ,(mapcar #'list args argvs)
       (let ((,@val (update ,getter ,index ,value)))
         ,setter
         ;; (dump ,getter)
         ))))

(defun update (dseg index value)
  (%update dseg
           index
           value
           0
           *range-end*))

(defun %update (dseg index value begin end)
  (when (> (abs (- begin end))
           1)
    (let ((mid (ash (+ begin end) -1)))
      (if (< index mid)
          (%make-dseg :val (if (null dseg)
                               value
                               (funcall *fn* value (dseg-val dseg)))
                      :l (%update (when dseg (dseg-l dseg))
                                  index
                                  value
                                  begin
                                  mid)
                      :r (when dseg (dseg-r dseg)))
          (%make-dseg :val (if (null dseg)
                               value
                               (funcall *fn* value (dseg-val dseg)))
                      :l (when dseg (dseg-l dseg))
                      :r (%update (when dseg (dseg-r dseg))
                                  index
                                  value
                                  mid
                                  end))))))
