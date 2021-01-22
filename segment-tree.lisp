;;;
;;; BOF
;;;

;; Segment-tree (1-indexed)

(defpackage :segment-tree
  (:nicknames :seg)
  (:use :cl)
  (:export :make-segment-tree
           :make-segment-tree-from-vector
           :update
           :get-val
           :ref))

(in-package :seg)

(defstruct (segment-tree (:conc-name seg-)
                         (:constructor %make-segment-tree))
  (fn nil :type function)
  (e nil)
  (m nil :type fixnum)
  (node nil :type simple-array))


(defun make-segment-tree (size &key (fn #'+) (e 0) (element-type 'fixnum))
  (let ((m (sb-int:named-let rec ((m 1))
             (if (>= m size)
                 m
                 (rec (ash m 1))))))
    (%make-segment-tree :fn fn
                        :e e
                        :m m
                        :node (make-array (ash m 1)
                                          :element-type element-type
                                          :adjustable nil
                                          :initial-element e))))

(defun make-segment-tree-from-vector (vector &key (fn #'+) (e 0))
  (let ((seg (make-segment-tree (length vector)
                                :fn fn
                                :e e
                                :element-type (array-element-type vector))))
    (loop for i of-type fixnum
          from (seg-m seg)
            below (ash (seg-m seg) 1)
          for x across vector
          do (setf (aref (seg-node seg) (+ i (seg-m seg)))
                   x))
    (loop for i of-type fixnum
          from (1- (seg-m seg))
          downto 1
          do (setf (aref (seg-node seg) i)
                   (funcall (seg-fn seg)
                            (aref (seg-node seg) (logior 0 (ash i 1)))
                            (aref (seg-node seg) (logior 1 (ash i 1))))))
    seg))


(defmethod get-val ((seg segment-tree)
                    (l fixnum)
                    (r fixnum))
  (let ((l (+ l (seg-m seg)))
        (r (+ r (seg-m seg))))
    (declare (fixnum l r))
    (loop while (< l r)
          with res of-type integer = (seg-e seg)
          when (logbitp 0 l)
            do (setf res
                     (funcall (seg-fn seg)
                              res
                              (aref (seg-node seg) l)))
            and do (incf l)
          when (logbitp 0 r)
            do (setf res
                     (funcall (seg-fn seg)
                              res
                              (aref (seg-node seg) (1- r))))
            and do (decf r)
          do (setq l (ash l -1))
             (setq r (ash r -1))
          finally
             (return res))))



(defmethod update ((seg segment-tree)
                   (i fixnum)
                   val)
  (let ((i (+ i (seg-m seg))))
    (setf (aref (seg-node seg) i)
          val)
    (let ((i (ash i -1)))
      (declare (fixnum i))
      (loop while (plusp i)
            do (setf (aref (seg-node seg) i)
                     (funcall (seg-fn seg)
                              (aref (seg-node seg) (logior 0 (ash i 1)))
                              (aref (seg-node seg) (logior 1 (ash i 1)))))
               (setq i (ash i -1))))))

(defmacro ref (seg i)
  `(aref (seg-node ,seg)
         (+ ,i (seg-m ,seg))))

(in-package :cl-user)

;;;
;;; EOF
;;;
