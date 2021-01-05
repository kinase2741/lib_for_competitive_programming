;;;
;;; BOF
;;;

(declaim (ftype (function (integer fixnum) integer) pow))
(defun pow (base power)
  (declare (integer base) (fixnum power))
  (loop with res of-type integer = 1
        while (plusp power) when (logbitp 0 power) do (setf res (* res base)) do (setf base (* base base) power (ash power -1))
        finally (return res)))

;;;
;;; EOF
;;;
