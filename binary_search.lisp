(defun lower-bound (vector x)
  ;; sortされたvectorに対して大小関係を壊さないように
  ;; xを挿入できる最小のindexを返す
  ;; 型宣言に注意
  (declare (vector vector)
           (fixnum x))
  (loop with lo of-type fixnum = -1
        with hi of-type fixnum = (length vector)
        with mid of-type fixnum = 0
        while (> (abs (- lo hi))
                 1)
        do (setq mid (floor (+ lo hi)
                            2))
        if (< (aref vector mid)
              x)
          do (setq lo mid)
        else
          do (setq hi mid)
        finally
           (return (1+ lo))))


(defun upper-bound (vector x)
  ;; sortされたvectorに対して大小関係を壊さないように
  ;; xを挿入できる最大のindexを返す
  ;; 型宣言に注意
  (declare (vector vector)
           (fixnum x))
  (loop with lo of-type fixnum = -1
        with hi of-type fixnum = (length vector)
        with mid of-type fixnum = 0
        while (> (abs (- lo hi))
                 1)
        do (setq mid (floor (+ lo hi)
                            2))
        if (> (aref vector mid)
              x)
          do (setq hi mid)
        else
          do (setq lo mid)
        finally
           (return hi)))
