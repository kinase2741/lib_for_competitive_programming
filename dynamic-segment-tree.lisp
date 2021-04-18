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
(defparameter fn (lambda (x y) (+ x y)))
(defparameter *max* (1- (ash 1 32)))
(defparameter *range-begin* 0)
(defparameter *range-end* (sb-int:named-let rec ((n0 1))
                            (if (>= n0 *max*)
                                n0
                                (rec (ash n0 1)))))


(defstruct (dseg-node (:conc-name dseg-)
                      (:constructor %make-dseg (&key (val op-identity) l r)))
  (val val)
  (l nil)
  (r nil))

(defun fold (dseg l r)
  (%fold dseg l r *range-begin* *range-end*))

(defun %fold (dseg l r begin end)
  (cond
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
