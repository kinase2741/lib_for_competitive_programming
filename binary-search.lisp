(defun lower-bound (fn ok ng)
  (loop while (> (abs (- ok ng)) 1)
        for mid = (ash (+ ok ng) -1)
        if (funcall fn mid)
          do (setq ok mid)
        else
          do (setq ng mid)
        finally
           (return ok)))

(defun upper-bound (fn ng ok)
  (loop while (> (abs (- ok ng)) 1)
        for mid = (ash (+ ok ng) -1)
        if (funcall fn mid)
          do (setq ok mid)
        else
          do (setq ng mid)
        finally
           (return ok)))
