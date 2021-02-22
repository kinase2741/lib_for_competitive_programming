;; RBST

(define-symbol-macro *op* #'+)

(defstruct (node (:constructor make-node (key value represent &optional left right)))
  (key 0 :type fixnum)
  (value 0 :type fixnum)
  (represent 0 :type fixnum)
  (cnt (+ 1
          (if (null left)
              0
              (node-cnt left))
          (if (null right)
              0
              (node-cnt right)))
   :type fixnum)
  (left nil :type (or null node))
  (right nil :type (or null node)))

;; for debug
(defun show (node)
  (let ((res nil))
    (labels ((rec (n)
               (when n
                 (with-slots ((k key)
                              (v value)
                              (l left)
                              (r right))
                     n
                   (rec l)
                   (push (format nil "~a:~a" k v) res)
                   (rec r)))))
      (rec node)
      (reverse res))))

(defun left (node)
  (when node (node-left node)))

(defun right (node)
  (when node (node-right node)))

(defun conc (l-node r-node)
  (unless l-node
    (return-from conc r-node))
  (unless r-node
    (return-from conc l-node))
  (with-slots ((lk key)
               (lv value)
               (lrep represent)
               (lc cnt)
               (ll left)
               (lr right))
      l-node
    (with-slots ((rk key)
                 (rv value)
                 (rrep represent)
                 (rc cnt)
                 (rl left)
                 (rr right))
        r-node
      (if (< (mod (random #.most-positive-fixnum)
                  (the fixnum (+ lc rc)))
             lc)
          ;; left is above
          ;; TODO:update is needed
          (make-node lk
                     lv
                     lrep
                     ll
                     (conc lr r-node))
          ;; right is above
          (make-node rk
                     rv
                     rrep
                     (conc l-node rl)
                     rr)))))

(defun split (node key)
  (unless node
    (return-from split (values nil nil)))
  (assert (<= 0 key (node-cnt node)))
  (with-slots ((lk key)
               (lv value)
               (lrep represent)
               (lc cnt))
      
    (with-slots ((rk key)
                 (rv value)
                 (rrep represent)
                 (rc cnt))
        (node-right node)
      (if (< key lc)
          (multiple-value-bind
                (ll lr)
              (split (node-left node) key)
            (values ll
                    (make-node lk
                               lv
                               lrep
                               lr
                               (node-right node))))
          (multiple-value-bind
                (rl rr)
              (split (node-right node) key)
            (values (make-node rk
                               rv
                               rrep
                               (node-left node)
                               rl)
                    rr))))))

(defun insert (node key value)
  (multiple-value-bind (l r) (split node key)
    (conc (conc l (make-node key value 0))
           r)))

(defun remove-key (node key)
  (multiple-value-bind (l c-r) (split node key)
    (multiple-value-bind (c r) (split c-r (1+ key))
      (declare (ignore c))
      (conc l r))))


;; prove
(ql:quickload :prove)

(prove:plan 3)

(prove:is-print )

(prove:finalize)
s
