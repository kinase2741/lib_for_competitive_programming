;;;
;;; BOF
;;;

;; Binary Trie (0-indexed)
;; make-bt, get-count : O(1)
;; insert, remove, get-val, get-min, get-max : O(+base+)
;; Reference: https://kazuma8128.hatenablog.com/entry/2018/05/06/022654

(in-package :cl-user)

(defpackage :binary-trie
  (:nicknames :bt)
  (:use :cl)
  (:shadow :remove)
  (:export :make-bt :get-size :insert :remove :get-kth-element :get-min :get-max :count-lower :count-value))

(in-package :binary-trie)

(defconstant +base+ 32)


(defstruct (binary-trie (:conc-name bt-)
                        (:constructor %make-bt (count &key left right)))
  ;; count : amount of child nodes
  (count 0 :type fixnum)
  (left nil :type (or null binary-trie))
  (right nil :type (or null binary-trie)))

(declaim (inline make-bt))
(defun make-bt () (%make-bt 0))

(declaim (inline empty-p))
(defun empty-p (bt) (or (null bt) (zerop (bt-count bt))))

(declaim (inline get-size))
(defun get-size (bt)
  (if (null bt) 0 (bt-count bt)))

(declaim (ftype (function ((or null binary-trie) fixnum &optional fixnum) (or null binary-trie)) insert))
(defun insert (bt value &optional (b #.(1- +base+)))
  (declare ((or null binary-trie) bt)
           (fixnum value b))
  (when (null bt)
    (setf bt (%make-bt 0)))
  (assert (not (null bt)))
  (incf (bt-count bt))
  (cond
    ((minusp b) bt)
    ((logbitp b value)
     (setf (bt-right bt) (insert (bt-right bt)
                                 value
                                 (1- b)))
     bt)
    (t (setf (bt-left bt) (insert (bt-left bt)
                                  value
                                  (1- b)))
       bt)))



(declaim (ftype (function ((or null binary-trie) fixnum &optional fixnum) (or null binary-trie)) remove))
(defun remove (bt value &optional (b #.(1- +base+)))
  (declare ((or null binary-trie) bt)
           (fixnum value b))
  (when (empty-p bt)
    (error "trie is empty or nil"))
  (decf (bt-count bt))
  (cond
    ((zerop (bt-count bt)) bt)
    ((minusp b) bt)
    ((logbitp b value)
     (setf (bt-right bt) (remove (bt-right bt)
                                 value
                                 (1- b)))
     bt)
    (t
     (setf (bt-left bt) (remove (bt-left bt)
                                value
                                (1- b)))
     bt)))


(declaim (ftype (function (binary-trie fixnum &optional fixnum) fixnum) get-kth-element))
(defun get-kth-element (bt k &optional (b #.(1- +base+)))
  (declare ((or null binary-trie) bt)
           (fixnum k b))
  (if (minusp b)
      0
      (let ((m (if (bt-left bt) (bt-count (bt-left bt)) 0)))
        (declare (fixnum m))
        (if (< k m)
            (get-kth-element (bt-left bt)
                             k
                             (1- b))
            (logior (get-kth-element (bt-right bt)
                                     (the fixnum (- k m))
                                     (1- b))
                    (ash 1 b))))))

(declaim (inline get-min get-max))
(defun get-min (bt)
  (get-kth-element bt 0))

(defun get-max (bt)
  (get-kth-element bt (1- (get-size bt))))

(defun count-lower (bt value &optional (b #.(1- +base+)))
  (cond
    ((or (null bt) (minusp b)) 0)
    ((logbitp 0 (ash value (- b)))
     (+ (if (bt-left bt)
            (bt-count (bt-left bt))
            0)
        (count-lower (bt-right bt)
                     value
                     (1- b))))
    (t
     (count-lower (bt-left bt)
                  value
                  (1- b)))))

(declaim (ftype (function ((or null binary-trie) fixnum) fixnum) count-value))
(defun count-value (bt value)
  (labels ((%count (bt value base)
             (cond
               ((minusp base)
                (get-size bt))
               ((logbitp 0 (ash value (- base)))
                (%count (bt-right bt)
                        value
                        (1- base)))
               (t
                (%count (bt-left bt)
                        value
                        (1- base))))))
    (%count bt value #.(1- +base+))))

(in-package :cl-user)

;;;
;;; EOF
;;;
