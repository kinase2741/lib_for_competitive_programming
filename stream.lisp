;;
;; BOF
;;

(declaim (ftype (function (sequence) simple-base-string) unwrap))
(defun unwrap (sequence)
  ;; Ex. (unwrap (list 1 2 3 4 5)) => "1 2 3 4 5"
  (let ((*standard-output* (make-string-output-stream :element-type 'base-char)))
    (let ((init nil))
      (declare (boolean init))
      (map nil
           (lambda (x)
             (when init
               (princ #\space))
             (setq init t)
             (princ x))
           sequence))
    (coerce (get-output-stream-string *standard-output*) 'simple-base-string)))

(defmacro with-buffered-stdout (&body body)
  (let ((out (gensym)))
    `(let ((,out (make-string-output-stream :element-type 'base-char)))
       (let ((*standard-output* ,out))
         ,@body)
       (write-string (get-output-stream-string ,out)))))

(declaim (ftype (function (string &optional base-char) list) split-string-into-list))
(defun split-string-into-list (string &optional (separator #\space))
  ;; 
  ;; Example.
  ;; 
  ;; (split-string-into-list "abc def ehi jk")
  ;;  ;=> ("abc" "def" "ehi" "jk")
  ;;
  (let ((pos (position #\space string)))
    (if pos
        (cons (subseq string 0 pos)
              (split-string-into-list (subseq string (1+ pos))
                                      separator))
        (list string))))


(declaim (ftype (function * (values fixnum &optional)) read-fixnum))
(defun read-fixnum (&optional (in *standard-input*))
  (declare (inline read-byte)
           #-swank (sb-kernel:ansi-stream in))
  (macrolet ((%read-byte ()
               `(the (unsigned-byte 8)
                     #+swank (char-code (read-char in nil #\Nul))
                     #-swank (read-byte in nil 0))))
    (let* ((minus nil)
           (result (loop (let ((byte (%read-byte)))
                           (cond ((<= 48 byte 57)
                                  (return (- byte 48)))
                                 ((zerop byte) ; #\Nul
                                  (error "Read EOF or #\Nul."))
                                 ((= byte #.(char-code #\-))
                                  (setq minus t)))))))
      (declare ((integer 0 #.most-positive-fixnum) result))
      (loop
        (let* ((byte (%read-byte)))
          (if (<= 48 byte 57)
              (setq result (+ (- byte 48) (the (integer 0 #.(floor most-positive-fixnum 10)) (* result 10))))
              (return (if minus (- result) result))))))))


;;
;; EOF
;;
