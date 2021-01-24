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
  (:export :make-bt :get-size :empty-p :insert :insert! :remove :remove! :get-element :get-min :get-max :lower-bound :upper-bound :count-value))

(in-package :binary-trie)

(defconstant +base+ 32)
(defconstant +base-minus+ (1- +base+))

(defstruct (binary-trie (:conc-name bt-)
                        (:constructor make-bt (&optional (count 0) left right)))
  (count 0 :type fixnum)
  (left nil :type (or null binary-trie))
  (right nil :type (or null binary-trie)))


(defun get-size (bt)
  (if (null bt)
      0
      (bt-count bt)))

(defun empty-p (bt)
  (zerop (get-size bt)))

;; New
(defun insert! (bt value)
  (loop for b of-type fixnum from +base-minus+ downto 0
        with bt-work of-type (or null binary-trie) = bt
        do (when (null bt-work)
             (setf bt-work (make-bt)))
           (incf (bt-count bt-work))
           (setf bt-work (if (logbitp 0 value)
                             (bt-right bt-work)
                             (bt-left bt-work))
                 value (ash value -1))))

(defun remove! (bt value)
  (loop for b of-type fixnum from +base-minus+ downto 0
        with bt-work of-type (or null binary-trie) = (if (null bt) nil (copy-structure bt))
        do (decf (bt-count bt-work))
           (when (empty-p bt-work)
             (setf bt nil)
             (return))
           (setf bt-work (if (logbitp 0 value)
                             (bt-right bt-work)
                             (bt-left bt-work))
                 value (ash value -1))))

(defun get-element (bt k)
  (loop for b from +base-minus+ downto 0
        with bt-work of-type (or null binary-trie) = (copy-structure bt)
        with acc of-type fixnum = 0
        do (let ((m (if (null (bt-left bt-work))
                        0
                        (get-size (bt-left bt-work)))))
             (setf acc (ash acc 1))
             (if (< k m)
                 (setf bt-work (bt-left bt-work))
                 (progn
                   (setf bt-work (bt-right bt-work)
                         k (- k m))
                   (setf acc (logior acc 1)))))
        finally (return acc)))


(declaim (inline get-min get-max))
(defun get-min (bt)
  (get-element bt 0))

(defun get-max (bt)
  (get-element bt (1- (get-size bt))))

(in-package :cl-user)

;;;
;;; EOF
;;;
