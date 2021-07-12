(defpackage sqrt-tree
  (:use :cl)
  (:nicknames :st))

(defstruct (sqrt-tree (:conc-name st-)
                      (:constructor %make-st))
  (main nil
   :type (simple-array fixnum (*)))
  (sub nil
   :type (simple-array fixnum (*)))
  ()
  (op nil :type function))

(defun build (size &Key op initial-element)
  (unless op
    (error "OP must be supplied.")))

(defun fold (st l r)
  nil)

(defun update (st l r)
  nil)
