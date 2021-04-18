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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *base-minus* 31))



(defstruct (node (:conc-name n-)
                 (:constructor make-n (cnt &key l r)))
  (cnt cnt)
  (l nil)
  (r nil))

(defun %add! (node val b)
  (cond
    ((minusp b) (make-n 1))
    (:else
     (let ((bool (logbitp b val)))
       ;; cntが1増える
       (make-n (1+ (if (null node)
                       0
                       (n-cnt node)))
               :l (if bool
                      (when node (n-l node))
                      (%add! (when node (n-l node))
                             val
                             (1- b)))
               :r (if bool
                      (%add! (when node (n-r node))
                             val
                             (1- b))
                      (when node (n-r node))))))))

(defmacro add! (binary-trie value)
  "Binary Trieにvalueを追加する。"
  (multiple-value-bind (args argvs val setter getter)
      (get-setf-expansion binary-trie)
    `(let ,(mapcar #'list args argvs)
       (let ((,@val (%add! ,getter ,value #.*base-minus*)))
         ,setter
         #+swank (dump ,binary-trie)))))

(defun %sub! (node val b)
  (cond
    ((or (minusp b)
         (= 1 (n-cnt node)))
     nil)
    (:else
     (assert node)
     (let ((bool (logbitp b val)))
       (make-n (1- (n-cnt node))
               :l (if bool
                      (n-l node)
                      (%sub! (n-l node)
                             val
                             (1- b)))
               :r (if bool
                      (%sub! (n-r node)
                             val
                             (1- b))
                      (n-r node)))))))


(defmacro sub! (binary-trie value)
  "Binary Trieからvalueを取り除く。"
  (multiple-value-bind (args argvs val setter getter)
      (get-setf-expansion binary-trie)
    `(let ,(mapcar #'list args argvs)
       (let ((,@val (%sub! ,getter ,value #.*base-minus*)))
         ,setter
         #+swank nil))))

(defun %ref (node index b)
  (cond
    ((minusp b) 0)
    (:else
     (let ((m (if (n-l node)
                  (n-cnt (n-l node))
                  0)))
       (if (< index m)
           (%ref (n-l node)
                 index
                 (1- b))
           (logior (%ref (n-r node)
                         (- index m)
                         (1- b))
                   (ash 1 b)))))))

(defun ref (binary-trie index)
  "Binary Trieに格納された値について、順に数えてindex番目のものを返す。"
  (%ref binary-trie index #.*base-minus*))


(defun get-size (binary-trie)
  "Binary Trieのサイズを返す。NILの場合は0を返す。"
  (if (null binary-trie)
      0
      (n-cnt binary-trie)))

(defun dump (binary-trie)
  "Binary-trieの各要素をリストに格納して返す。デバッグ用。"
  (loop for index from 0 below (get-size binary-trie)
        collect (ref binary-trie index)))

(in-package :cl-user)
