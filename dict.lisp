

(defmacro href (hash-table &rest args &key default)
  `(gethash ,args ,hash-table ,default))


(defmacro set-alias-for-function (fn alias)
  `(setf (symbol-function ,alias)
         ,fn))

(defmacro make-list-hash (&key size 100)
  `(make-hash-table :test #'equal
                    :size 10000))

(defmacro hset (hash-table key &rest args)
  `(setf (gethash ,key ,hash-table)
         ,args))


(defmacro h)
