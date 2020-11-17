(defparameter *inf* 1000000000)

(defun bit-dp (fn node-amount)
  ;;
  ;; Example: https://atcoder.jp/contests/abc180/submissions/17726503
  ;;  
  ;;  (bit-dp (lambda (node next)
  ;;            (calc-cost node next))
  ;;          node-amount)
  ;;
  ;;
  (let ((memo (make-array (* (+ node-amount 10)
                             (+ (ash 1 node-amount) 10))
                          :element-type 'integer
                          :initial-element *inf*
                          :adjustable nil)))
    (declare ((array integer 1) memo))
    (labels ((encode (visited node)
               (+ (* visited (+ node-amount 5))
                  node)))
      (sb-int:named-let rec ((visited 0)
                             (node 0))
        (declare (fixnum visited node))
        (let ((val (aref memo (encode visited node))))
          (cond
            ((< val *inf*) val)
            ;; すべての頂点を訪れて頂点に戻ってきた場合
            ((and (= visited (1- (ash 1 node-amount)))
                  (zerop node))
             0)
            ;; すべて回る前に戻ってきてしまった場合 -> 無効
            ((and (/= visited 0)
                  (= node 0))
             *inf*)
            (t
             (setf (aref memo (encode visited node))
                   (reduce #'min
                           (mapcar (lambda (next)
                                     (+ (rec (logior visited
                                                     (ash 1 next))
                                             next)
                                        (funcall fn node next)))
                                   (remove-if (lambda (next)
                                                (= (logand (ash visited (- next))
                                                           1)
                                                   1))
                                              (loop for i below node-amount collect i))))))))))))


