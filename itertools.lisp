(declaim (ftype (function (sequence &optional fixnum) list) product)
         (ftype (function (sequence &optional list fixnum) *) %product))
(defun product (sequence &optional (repeat 1))
  (let ((res nil))
    (labels ((%product (seq &optional (acc nil) (cnt 0))
               (if (= cnt repeat)
                   (push (reverse acc) res)
                   (map nil
                        (lambda (x)
                          (%product seq
                                    (cons x acc)
                                    (1+ cnt)))
                        seq))))
      (%product sequence)
      (reverse res))))

(declaim (ftype (function (list &optional fixnum) list) permutations-for-list)
         (ftype (function (list fixnum list) list) %perm-list))
(defun permutations-for-list (list &optional (r (length list)))
  (let ((res nil))
    (labels ((%perm-list (xs r acc)
               (if (null xs)
                   (push (reverse acc) res)
                   (mapc (lambda (x)
                           (%perm-list (remove x xs :count 1)
                                       (1- r)
                                       (cons x acc)))
                         xs))))
      (%perm-list list r nil)
      (reverse res))))

