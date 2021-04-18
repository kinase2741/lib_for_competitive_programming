;;;
;;; BOF
;;;

;; Binary Trie (0-indexed)
;; Reference: https://kazuma8128.hatenablog.com/entry/2018/05/06/022654

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *type* 'fixnum)
  (defparameter *base* 32) ; 格納したい値のサイズによって調整
  (defparameter *base-minus* (1- *base*))
  (defparameter *maybe-trie* '(or null node)))

(defstruct (node (:conc-name n-)
                 (:constructor make-n (cnt &key l r)))
  (cnt cnt :type #.*type*)
  (l nil :type #.*maybe-trie*)
  (r nil :type #.*maybe-trie*))

(defun bt-add (binary-trie value)
  "binary-trieにvalueを追加したものを返す。"
  (%add binary-trie value #.*base-minus*))

(defmacro bt-add! (binary-trie value)
  "破壊的にbinary-trieにvalueを追加する。REPL上ではListにdumpしたものを返す。"
  (multiple-value-bind (args argvs val setter getter)
      (get-setf-expansion binary-trie)
    `(let ,(mapcar #'list args argvs)
       (let ((,@val (bt-add ,getter ,value)))
         ,setter
         #+swank (dump ,binary-trie)))))

(defun bt-sub (binary-trie value)
  "binary-trieからvalueを削除したものを返す。"
  (declare (#.*maybe-trie* binary-trie)
           (#.*type* value))
  (%sub binary-trie value #.*base-minus*))

(defmacro bt-sub! (binary-trie value)
  "破壊的にbinary-trieからvalueを取り除く。REPL上ではListにdumpしたものを返す。"
  (multiple-value-bind (args argvs val setter getter)
      (get-setf-expansion binary-trie)
    `(let ,(mapcar #'list args argvs)
       (let ((,@val (bt-sub ,getter ,value)))
         ,setter
         #+swank (dump ,binary-trie)))))

(defun bt-ref (binary-trie index)
  "Binary Trieに格納された値について、順に数えてindex番目のものを返す。"
  (declare (#.*maybe-trie* binary-trie)
           (#.*type* index))
  (%ref binary-trie index #.*base-minus*))

(declaim (inline bt-get-size))
(defun bt-get-size (binary-trie)
  "Binary Trieのサイズを返す。NILの場合は0を返す。"
  (declare (#.*maybe-trie* binary-trie))
  (the #.*type*
       (if (null binary-trie)
           0
           (n-cnt binary-trie))))

(defun bt-dump (binary-trie)
  "Binary-trieの各要素をリストに格納して返す。デバッグ用。"
  (loop for index from 0 below (bt-get-size binary-trie)
        collect (bt-ref binary-trie index)))

;; Helper functions

(defun %add (node val b)
  (declare (#.*maybe-trie* node)
           (#.*type* val)
           ((integer -1 #.*base*) b))
  (the #.*maybe-trie*
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
                           (%add (when node (n-l node))
                                 val
                                 (1- b)))
                    :r (if bool
                           (%add (when node (n-r node))
                                 val
                                 (1- b))
                           (when node (n-r node)))))))))

(defun %sub (node val b)
  (declare (#.*maybe-trie* node)
           (#.*type* val)
           ((integer -1 #.*base*) b))
  (the #.*maybe-trie*
       (cond
         ((or (minusp b)
              (= 1 (n-cnt node)))
          nil)
         (:else
          #+swank (assert node)
          (let ((bool (logbitp b val)))
            (make-n (1- (n-cnt node))
                    :l (if bool
                           (n-l node)
                           (%sub (n-l node)
                                 val
                                 (1- b)))
                    :r (if bool
                           (%sub (n-r node)
                                 val
                                 (1- b))
                           (n-r node))))))))

(defun %ref (node index b &optional (acc 0))
  (declare (#.*maybe-trie* node)
           (#.*type* index acc)
           ((integer -1 #.*base*) b))
  (the #.*type*
       (cond
         ((minusp b) acc)
         (:else
          (let ((m (if (n-l node)
                       (n-cnt (n-l node))
                       0)))
            (if (< index m)
                (%ref (n-l node)
                      index
                      (1- b)
                      acc)
                (%ref (n-r node)
                      (- index m)
                      (1- b)
                      (logior acc
                              (ash 1 b)))))))))
