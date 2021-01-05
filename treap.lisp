;;;
;;; BOF
;;;

;; Treap

(defpackage :treap
  (:use :cl)
  (:shadow :count :find :merge :remove)
  (:export :make-treap :insert :remove :get-min))

(in-package :treap)

(defstruct (treap (:conc-name %treap-)
                  (:constructor %make-treap (value priority cnt &key left right)))
  (value 0 :type fixnum)
  (left nil :type (or null treap))
  (right nil :type (or null treap))
  (priority 0 :type (integer 0 #.most-positive-fixnum))
  (cnt 0 :type fixnum))

(declaim (inline make-treap))
(defun make-treap () nil)

(defun count (treap)
  (if (null treap) 0 (%treap-cnt treap)))

(defun update (treap)
  (setf (%treap-cnt treap)
        (+ (count (%treap-left treap))
           (count (%treap-right treap))
           1))
  treap)

(declaim (ftype (function ((or null treap) fixnum) (or null fixnum)) find))
(defun find (treap value)
  (cond
    ((null treap) nil)
    ((= value (%treap-key treap)) (%treap-value treap))
    ((< value (%treap-key treap)) (find (%treap-left treap)
                                        value))
    (t (find (%treap-right treap)
             value))))

(declaim (ftype (function ((or null treap) (or null treap))
                          (or null treap))
                merge))
(defun merge (left right)
  (flet ((%make (main left right)
           (%make-treap (%treap-value main)
                        (%treap-priority main)
                        
                        :left left
                        :right right)))
    (cond ((null left) right)
          ((null right) left)
          ((> (%treap-priority left)
              (%treap-priority right))
           (%make left
                  (%treap-left left)
                  (merge (%treap-right left)
                         right)))
          (t
           (%make right 
                  :left (merge (%treap-left right)
                                     left)
                        :right (%treap-right right))))))

(declaim (ftype (function ((or null treap) fixnum) (values (or null treap) (or null treap))) split))
(defun split (treap value)
  (cond ((null treap) (values nil nil))
        ((< value (%treap-value treap))
         (multiple-value-bind (l-left l-right)
             (split (%treap-left treap) value)
           (values l-left
                   (%make-treap (%treap-value treap)
                                (%treap-priority treap)
                                (%treap-cnt treap)
                                :left l-right
                                :right (%treap-right treap)))))
        (t
         (multiple-value-bind (r-left r-right)
             (split (%treap-right treap) key)
           (values (%make-treap (%treap-key treap)
                                (%treap-value treap)
                                (%treap-priority treap)
                                :left (%treap-left treap)
                                :right r-left)
                   r-right)))))

(declaim (ftype (function ((or null treap) fixnum fixnum)
                          (or null treap))
                insert))
(defun insert (treap key value)
  (multiple-value-bind (left right) (split treap key)
    (merge (merge left
                  (%make-treap key value (random most-positive-fixnum)))
           right)))

(declaim (ftype (function ((or null treap) fixnum)
                          (or null treap))
                remove))
(defun remove (treap key)
  (cond
    ((null treap) nil)
    ((= key (%treap-key treap))
     (merge (%treap-left treap)
            (%treap-right treap)))
    ((< key (%treap-key treap))
     (%make-treap (%treap-key treap)
                  (%treap-value treap)
                  (%treap-priority treap)
                  :left (remove (%treap-left treap) key)
                  :right (%treap-right treap)))
    (t
     (%make-treap (%treap-key treap)
                  (%treap-value treap)
                  (%treap-priority treap)
                  :left (%treap-left treap)
                  :right (remove (%treap-right treap) key)))))


;; TODO:ensure-exist

(defun get-element (treap index)
  (cond
    ((null (%treap-left treap))
     (%treap-value ))))

(declaim (ftype (function ((or null treap)) (values (or null fixnum)
                                                    (or null fixnum)))
                get-min))
(defun get-min (treap)
  (cond
    ((null treap) nil)
    ((null (%treap-left treap))
     (values (%treap-key treap)
             (%treap-value treap)))
    (t (get-min (%treap-left treap)))))

(in-package :cl-user)

;;;
;;; EOF
;;;
