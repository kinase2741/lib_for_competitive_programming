(defun sieve (size)
  ;; 0~sizeまでの素数判定を行う
  ;; 結果をarrayとして返す
  ;;
  ;; Ex. (defparameter a (sieve 100)) => A
  ;;     (aref a 17) => T   ;; 17は素数
  ;;
  (let ((is-prime (make-array (1+ size) :element-type 'boolean
                                        :adjustable nil
                                        :initial-element t)))
    (setf (aref is-prime 0) nil)
    (setf (aref is-prime 1) nil)
    (loop for i from 2 to size do
      (when (aref is-prime i)
        (loop for j from 2 to (floor size i) do
          (setf (aref is-prime (* i j)) nil))))
    is-prime))
