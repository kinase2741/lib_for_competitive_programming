;;
;; BOF
;;

(declaim (ftype (function (sequence) simple-base-string) unwrap))
(defun unwrap (sequence)
  ;; e.g. (unwrap (list 1 2 3 4 5)) => "1 2 3 4 5"
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
  ;; Quoted from: https://competitive12.blogspot.com/2020/03/common-lisp.html
  (let ((out (gensym)))
    `(let ((,out (make-string-output-stream :element-type 'base-char)))
       (let ((*standard-output* ,out))
         ,@body)
       (write-string (get-output-stream-string ,out)))))

(declaim (inline read-fixnum read-nums println))
(defun read-fixnum (&optional (in *standard-input*))
  ;; Ref: https://competitive12.blogspot.com/2020/03/common-lisp.html
  ;;        partially modified
  (declare (inline read-byte))
  (flet ((%read-byte ()
           (the fixnum #+swank (char-code (read-char in nil #\Nul))
                       #-swank (read-byte in nil #.(char-code #\Nul))))
         (%byte->num (b)
           (the fixnum (- b #.(char-code #\0))))
         (%digit-p (byte)
           (declare (fixnum byte))
           (<= #.(char-code #\0) byte #.(char-code #\9))))
    (declare (inline %read-byte %byte->num %digit-p))
    (let ((minus nil)
          (res 0))
      (declare (boolean minus) (fixnum res))
      (loop for byte of-type fixnum = (%read-byte)
            do (cond
                 ((%digit-p byte)
                  (setf res (%byte->num byte))
                  (return))
                 ((= byte #.(char-code #\Nul))
                  (error "EOF"))
                 ((= byte #.(char-code #\-))
                  (setf minus t))))
      (loop for byte of-type fixnum = (%read-byte)
            do (cond
                 ((%digit-p byte)
                  (setf res (the fixnum (+ (* res 10) (%byte->num byte)))))
                 (t (return))))
      (the fixnum (if minus (- res) res)))))

(defun set! (arr count)
  (dotimes (i count)
    (setf (aref arr i)
          (read-fixnum))))

(defun read-base-char (&optional (in *standard-input*) (eof #\Newline))
  (declare (inline read-byte)
           #-swank (sb-kernel:ansi-stream in)
           (base-char eof))
  #+swank (coerce (read-char in nil eof) 'base-char)
  #-swank
  (the base-char (code-char (the (integer 0 127) (read-byte in nil (char-code eof))))))

(defmacro read-line! (&optional (buffer-size 20) (in *standard-input*) (term #\Newline))
  
  (let ((res (gensym))
        (c (gensym))
        (i (gensym)))
    `(let ((,res (load-time-value (make-string ,buffer-size :element-type 'base-char))))
       (declare (simple-base-string ,res)
                (inline read-base-char))
       (loop for ,c of-type base-char = (read-base-char ,in)
             for ,i of-type fixnum below ,buffer-size
             until (char= ,c ,term) do (setf (schar ,res ,i)
                                             ,c))
       ,res)))

(defun split (string &optional (separator #\space))
  (declare (base-string string)
           (base-char separator))
  (let ((pos (position separator string)))
    (if pos
        (cons (subseq string 0 pos)
              (split (subseq string (1+ pos))
                     separator))
        (list string))))

;;
;; EOF
;;
