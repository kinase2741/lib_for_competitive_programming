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
  (:export :make-bt :get-size :empty-p :insert :remove :get-kth-element :get-min :get-max :count-lower :count-value))

(in-package :binary-trie)

(defconstant +base+ 32)
(defconstant +base-minus+ (1- +base+))


(defun make-bt (&optional (count 0) left right)
  (list count left right))

(defun get-size (bt)
  (if (null bt) 0 (bt-count bt)))

(defun empty-p (bt)
  (zerop (get-size bt)))

(defun bt-count (bt) (if (null bt) 0 (first bt)))
(defun bt-left (bt) (if (null bt) nil (second bt)))
(defun bt-right (bt) (if (null bt) nil (third bt)))


(defun insert (bt value)
  (labels ((%insert (bt value b)
             (let ((bt (if (null bt) (make-bt) bt)))
               (if (minusp b)
                   (make-bt (1+ (bt-count bt))
                            (bt-left bt)
                            (bt-right bt))
                   (if (logbitp b value)
                       (make-bt (1+ (bt-count bt))
                                (bt-left bt)
                                (%insert (bt-right bt)
                                         value
                                         (1- b)))
                       (make-bt (1+ (bt-count bt))
                                (%insert (bt-left bt)
                                         value
                                         (1- b))
                                (bt-right bt)))))))
    (%insert bt value +base-minus+)))

(defun remove (bt value)
  (labels ((%remove (bt value b)
             (if (minusp b)
                 (make-bt (1- (bt-count bt))
                          (bt-left bt)
                          (bt-right bt))
                 (if (logbitp b value)
                     (make-bt (1- (bt-count bt))
                              (bt-left bt)
                              (%remove (bt-right bt)
                                       value
                                       (1- b)))
                     (make-bt (1- (bt-count bt))
                              (%remove (bt-left bt)
                                       value
                                       (1- b))
                              (bt-right bt))))))
    (%remove bt value +base-minus+)))


(defun get-element (bt k)
  (labels ((%get-value (bt k b acc)
             (if (minusp b)
                 acc
                 (let ((m (if (null (bt-left bt))
                              0
                              (bt-count (bt-left bt)))))
                   (if (< k m)
                       (%get-value (bt-left bt)
                                   k
                                   (1- b)
                                   acc)
                       (%get-value (bt-right bt)
                                   (- k m)
                                   (1- b)
                                   (logior acc (ash 1 b))))))))
    (%get-value bt k +base-minus+ 0)))

(defun get-min (bt)
  (get-element bt 0))

(defun get-max (bt)
  (get-element bt (1- (get-size bt))))

(defun lower-bound (bt value)
  (labels ((%count-lower (bt b acc)
             (cond
               ((or (empty-p bt)
                    (minusp b))
                acc)
               ((logbitp b value)
                (%count-lower (bt-right bt)
                              (1- b)
                              (+ acc (if (empty-p (bt-left bt))
                                         0
                                         (bt-count (bt-left bt))))))
               (t
                (%count-lower (bt-left bt)
                              (1- b)
                              acc)))))
    (%count-lower bt +base-minus+ 0)))

(defun upper-bound (bt value)
  (lower-bound bt (1+ value)))

(in-package :cl-user)

;;;
;;; EOF
;;;
