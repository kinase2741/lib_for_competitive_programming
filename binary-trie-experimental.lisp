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

(declaim (ftype (function (&optional fixnum list list) list) make-bt)
         (inline make-bt))
(defun make-bt (&optional (count 0) left right)
  (list count left right))

(defun get-size (bt)
  (bt-count bt))

(declaim (ftype (function (list) boolean) empty-p)
         (inline empty-p))
(defun empty-p (bt)
  (zerop (get-size bt)))

(declaim (inline bt-count bt-left bt-right))
(defun bt-count (bt) (if (null bt) 0 (first bt)))
(defun bt-left (bt) (if (null bt) nil (second bt)))
(defun bt-right (bt) (if (null bt) nil (third bt)))

(declaim (ftype (function (list fixnum) list) insert))
(defun insert (bt value)
  (declare (list bt)
           (fixnum value))
  (labels ((%insert (bt value b)
             (declare (list bt)
                      (fixnum value b))
             (let ((bt (if (null bt) (make-bt) bt)))
               (declare (list bt))
               (the list
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
                                     (bt-right bt))))))))
    (declare (ftype (function (list fixnum fixnum) list) %insert))
    (%insert bt value +base-minus+)))

(declaim (ftype (function (list fixnum) list) remove))
(defun remove (bt value)
  (declare (list bt)
           (fixnum value))
  (labels ((%remove (bt value b)
             (declare (list bt)
                      (fixnum value b))
             (the list
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
                                   (bt-right bt)))))))
    (declare (ftype (function (list fixnum fixnum) list) %remove))
    (%remove bt value +base-minus+)))

(define-modify-macro insert! (value) (lambda (bt value) (insert bt value)))
(define-modify-macro remove! (value) (lambda (bt value) (remove bt value)))

(declaim (ftype (function (list fixnum) fixnum) get-element))
(defun get-element (bt k)
  (declare (list bt)
           (fixnum k))
  (labels ((%get-value (bt k b acc)
             (declare (list bt)
                      (fixnum k b acc))
             (the fixnum
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
                                        (logior acc (ash 1 b)))))))))
    (declare (ftype (function (list fixnum fixnum fixnum) fixnum) %get-value))
    (%get-value bt k +base-minus+ 0)))

(declaim (inline get-min get-max))
(defun get-min (bt)
  (get-element bt 0))

(defun get-max (bt)
  (get-element bt (1- (bt-count bt))))

(declaim (ftype (function (list fixnum) fixnum) lower-bound))
(defun lower-bound (bt value)
  (declare (list bt)
           (fixnum value))
  (labels ((%count-lower (bt b acc)
             (declare (list bt)
                      (fixnum b acc))
             (the fixnum
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
                                   acc))))))
    (declare (ftype (function (list fixnum fixnum) fixnum) %count-lower))
    (%count-lower bt +base-minus+ 0)))

(declaim (inline upper-bound))
(defun upper-bound (bt value)
  (lower-bound bt (1+ value)))

(defun count-value (bt value)
  (labels ((%count (bt b)
             (cond
               ((empty-p bt) 0)
               ((minusp b)
                (bt-count bt))
               ((logbitp b value)
                (%count (bt-right bt)
                        (1- b)))
               (t
                (%count (bt-left bt)
                        (1- b))))))
    (%count bt +base-minus+)))

(in-package :cl-user)

;;;
;;; EOF
;;;
