;; BOF

;; Binary-indexed-tree (1-indexed)
;; Reference: http://hos.ac/slides/20140319_bit.pdf

(defpackage :binary-indexed-tree
  (:nicknames :bit)
  (:use :cl)
  (:export :make-binary-indexed-tree
           :add
           :get-val
           :get-range-val
           :lower-bound))

(in-package :bit)

(defstruct (binary-indexed-tree (:conc-name bit-)
                                (:constructor %make-bit))
  (size nil :type fixnum)
  (dat nil :type (simple-array integer 1)))

(defmethod make-binary-indexed-tree ((size fixnum))
  (%make-bit :size size
             :dat (make-array (1+ size)
                              :element-type 'integer
                              :adjustable nil
                              :initial-element 0)))

(defmethod add ((index fixnum)
                (x integer)
                (bit binary-indexed-tree))
  (assert (<= 1 index (bit-size bit)))
  (labels ((rec (i)
             (when (<= i (bit-size bit))
               (incf (aref (bit-dat bit) i) x)
               (rec (+ i (logand i (- i)))))))
    (rec index)))

(defmethod get-val ((index fixnum)
                    (bit binary-indexed-tree))
  "get sum of value in [1,index)"
  (assert (<= 0 index (bit-size bit)))
  (labels ((rec (i acc)
             (if (<= i 0)
                 acc
                 (rec (- i (logand i (- i)))
                      (+ acc
                         (aref (bit-dat bit) i))))))
    (rec index 0)))

(defmethod get-range-val ((l fixnum)
                          (r fixnum)
                          (bit binary-indexed-tree))
  (- (get-val (1- r) bit)
     (get-val (1- l) bit)))

(defmethod lower-bound ((w integer)
                        (bit binary-indexed-tree))
  "a_1 + a_2 + ... + a_i >= wとなる最小のiを返す"
  (if (<= w 0)
      0
      (labels ((%get-range (r)
                 (if (> r (bit-size bit))
                     (ash r -1)
                     (%get-range (ash r 1))))
               (%lower-bound (range w acc)
                 (cond ((<= range 0) acc)
                       ((and (<= (+ range acc)
                                 (bit-size bit))
                             (< (aref (bit-dat bit) (+ range acc))
                                w))
                        (%lower-bound (ash range -1)
                                      (- w (aref (bit-dat bit) (+ range acc)))
                                      (+ acc range)))
                       (t
                        (%lower-bound (ash range -1)
                                      w
                                      acc)))))
        (1+ (%lower-bound (%get-range 1)
                          w
                          0)))))

(in-package :cl-user)

;; EOF
