;;;
;;; BOF
;;;

;; Splay tree

(defpackage :splay
  (:use :cl)
  (:shadow :merge :find :remove)
  (:export :make-splay :insert :remove))

(in-package :splay)

(defstruct (splay-node (:conc-name sp-)
                       (:constructor %make-node (value &key parent left right)))
  (value 0 :type fixnum)
  (size 1 :type fixnum)
  (parent nil :type (or null splay-node))
  (left nil :type (or null splay-node))
  (right nil :type (or null splay-node)))

(defmacro maybe-splay () '(or null splay-node))

(defun rotate (sp)
  (let* ((p (sp-parent sp))
         (pp (sp-parent p))
         (c (sp-right sp)))

    (cond
      ((equal (sp-left p) sp)
       )
      (t
       ))
    
    (when (and pp
               (equal (sp-left pp)
                      p))
      (setf (sp-left pp) sp))
    (when (equal (sp-right pp)
                 p)
      (setf (sp-right pp) sp))
    (setf (sp-parent p) sp
          (sp-left p) c)
    (when c
      (setf (sp-parent c) p))))

(defun splay ())

(defun update (sp)
  (setf (sp-size sp) 1)
  (when (sp-left sp)
    (incf (sp-size sp)
          (sp-size (sp-left sp))))
  (when (sp-right sp)
    (incf (sp-size sp)
          (sp-size (sp-right sp)))))

