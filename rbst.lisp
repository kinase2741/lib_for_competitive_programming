;;;
;;; BOF
;;;

;; RBST
;; Reference : https://www.slideshare.net/iwiwi/2-12188757

(in-package :cl-user)

(defpackage :rbst
  (:use :cl)
  (:shadow :find :remove :count :update :merge :replace)
  (:export :make-rbst :count :operate :get-element :insert :remove :replace :merge :split))

(in-package :rbst)

(defconstant +e+ most-positive-fixnum)
(defconstant +fn+ #'min)

(defstruct (rbst (:constructor %make-rbst (value cnt lazy &key l r)))
  (value 0)
  (cnt 0)
  (l nil)
  (r nil)
  (op +e+))

(defmacro with-null-checks ((&rest objs) &body body)
  `(cond
     ,@(mapcar (lambda (obj)
                 `((null ,obj) (error "~a is NIL" ',obj)))
               objs)
     (t ,@body)))

(defun make-rbst () nil)

(defun count (rbst)
  (if (null rbst) 0 (rbst-cnt rbst)))


(defun operate (rbst)
  (cond
    ((null rbst) +e+)
    (t (funcall +fn+
                (operate (rbst-l rbst))
                (operate (rbst-r rbst))
                (rbst-value rbst)))))

(defun update (rbst)
  (%make-rbst (rbst-value rbst)
              (+ (count (rbst-l rbst))
                 (count (rbst-r rbst))
                 1)
              (funcall +fn+
                       (rbst-value rbst)
                       (operate (rbst-l rbst))
                       (operate (rbst-r rbst)))
              :l (rbst-l rbst)
              :r (rbst-r rbst)))

(defun merge (l r)
  (cond
    ((null l) r)
    ((null r) l)
    (t
     (let* ((l-cnt (rbst-cnt l))
            (r-cnt (rbst-cnt r))
            (random-val (random (+ l-cnt r-cnt))))
       (if (< random-val l-cnt)
           ;; make l parent
           (let ((l-l (rbst-l l))
                 (l-r (update (merge (rbst-r l)
                                     r))))
             (update (%make-rbst (rbst-value l)
                                 1
                                 (rbst-value l)
                                 :l l-l
                                 :r l-r)))
           ;; make r parent
           (let ((r-l (merge l
                             (rbst-l r)))
                 (r-r (rbst-r r)))
             (update (%make-rbst (rbst-value r)
                                 1
                                 (rbst-value r)
                                 :l r-l
                                 :r r-r))))))))

(defun split (rbst key)
  (cond
    ((null rbst) (values nil nil))
    ((<= key (count (rbst-l rbst)))
     (multiple-value-bind (former latter) (split (rbst-l rbst) key)
       ;; l => latter, r => r
       (let ((rbst (update (%make-rbst (rbst-value rbst)
                                       1
                                       (rbst-value rbst)
                                       :l latter
                                       :r (rbst-r rbst)))))
         (values former rbst))))
    (t
     (multiple-value-bind (former latter) (split (rbst-r rbst) (- key (count (rbst-l rbst)) 1))
       ;; l => l, r => former
       (let ((rbst (update (%make-rbst (rbst-value rbst)
                                       1
                                       (rbst-value rbst)
                                       :l (rbst-l rbst)
                                       :r former))))
         (values rbst latter))))))

(defun insert (rbst key value)
  (multiple-value-bind (l r) (split rbst key)
    (merge (merge l
                  (%make-rbst value
                              1
                              value))
           r)))


(defun remove (rbst key)
  (multiple-value-bind (l c-r) (split rbst key)
    (multiple-value-bind (c r) (split c-r 1)
      (declare (ignore c))
      (merge l r))))


(defun get-element (rbst key)
  (multiple-value-bind (l c-r) (split rbst key)
    (declare (ignore l))
    (multiple-value-bind (c r) (split c-r 1)
      (declare (ignore r))
      (rbst-value c))))



(in-package :cl-user)

;;;
;;; EOF
;;;
