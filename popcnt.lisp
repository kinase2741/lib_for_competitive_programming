(in-package :cl-user)

(defpackage :popcnt 
  (:use :cl)
  (:export :popcnt))

(in-package :popcnt)

(sb-c:defknown popcnt (fixnum)
    (integer 0 64)
    (sb-c:movable sb-c:flushable sb-c:foldable)
  :overwrite-fndb-silently t)

(in-package :sb-vm)
(define-vop (popcnt::popcnt)
  (:translate popcnt:popcnt)
  (:policy :fast-safe)
  )

