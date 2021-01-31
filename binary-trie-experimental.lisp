;;;
;;; BOF
;;;

;; Binary Trie (0-indexed)
;; Reference: https://kazuma8128.hatenablog.com/entry/2018/05/06/022654

(in-package :cl-user)

(defconstant +base+ 32)
(defconstant +base-minus+ (1- +base+))

(deftype maybe-trie () '(or null binary-trie))

(defstruct (binary-trie (:conc-name bt-)
                        (:constructor make-bt (&optional (count 0) left right)))
  (count 0 :type fixnum)
  (left nil :type maybe-trie)
  (right nil :type maybe-trie))


(defmacro %defun (fn-spec args-spec &body body)
  `(progn
     (declaim (ftype (function (,@(mapcar #'second args-spec)) ,(second fn-spec)) ,(first fn-spec)))
     (defun ,(first fn-spec) (,@(mapcar #'first args-spec))
       (declare ,@(mapcar (lambda (args)
                            `(type ,(second args) ,(first args)))
                          args-spec))
       (the ,(second fn-spec) ,@body))))
(defmacro %nlet (fn-spec args-spec &body body)
  ;; (x 1 fixnum)
  `(labels ((,(first fn-spec) (,@(mapcar #'first args-spec))
              (declare ,@(mapcar (lambda (args)
                                   `(type ,(third args) ,(first args)))
                                 args-spec))
              (the ,(second fn-spec)
                   ,@body)))
     (declare (ftype (function (,@(mapcar #'third args-spec)) ,(second fn-spec)) ,(first fn-spec)))
     (,(first fn-spec) ,@(mapcar #'second args-spec))))

(declaim (inline get-size empty-p))
(%defun (get-size fixnum) ((bt maybe-trie))
  (if (null bt)
      0
      (bt-count bt)))

(%defun (empty-p boolean) ((bt maybe-trie))
  (zerop (get-size bt)))

(%defun (%insert binary-trie) ((bt maybe-trie)
                               (value fixnum))
  (%nlet (rec binary-trie) ((b +base-minus+ fixnum)
                            (bt bt maybe-trie))
    (let ((bt (or bt (make-bt))))
      (with-slots (count left right) bt
        (cond
          ((minusp b) (or bt (make-bt)))
          ((logbitp b value)
           (make-bt (1+ count)
                    left
                    (rec (1- b)
                         right)))
          (t
           (make-bt (1+ count)
                    (rec (1- b)
                         left)
                    right)))))))

(%defun (%remove maybe-trie) ((bt maybe-trie)
                              (value fixnum))
  (%nlet (rec maybe-trie) ((b +base-minus+ fixnum)
                           (bt bt maybe-trie))
    (let ((bt (or bt (make-bt))))
      (with-slots (count left right) bt
        (cond
          ((minusp b) bt)
          ((logbitp b value)
           (make-bt (1- count)
                    left
                    (rec (1- b)
                         right)))
          (t
           (make-bt (1- count)
                    (rec (1- b)
                         left)
                    right)))))))

(define-modify-macro insert! (value) (lambda (bt value) (%insert bt value)))
(define-modify-macro remove! (value) (lambda (bt value) (%remove bt value)))


(%defun (get-element fixnum) ((bt maybe-trie)
                              (k fixnum))
  (%nlet (rec fixnum) ((b +base-minus+ fixnum)
                       (bt bt maybe-trie)
                       (k k fixnum)
                       (acc 0 fixnum))
    (let ((bt (or bt (make-bt))))
      (with-slots (count left right) bt
        (let ((m (if (null left)
                     0
                     (get-size left))))
          (declare (fixnum m))
          (cond
            ((minusp b) acc)
            ((< k m) (rec (the fixnum (1- b))
                          left
                          k
                          acc))
            (t (rec (the fixnum (1- b))
                    right
                    (- k m)
                    (logior acc (ash 1 b))))))))))

(declaim (inline get-min get-max))
(%defun (get-min fixnum) ((bt maybe-trie))
  (get-element bt 0))

(%defun (get-max fixnum) ((bt maybe-trie))
  (get-element bt (the fixnum (1- (get-size bt))))) 

;;;
;;; EOF
;;;
