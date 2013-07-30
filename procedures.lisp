(defparameter *procedures* (make-hash-table))

(defmacro defprocedure (func &body body)
  "make primitive procedures"
  `(setf (gethash ,func *procedures*) (lambda (params)
					(let ((output ( temp-register-allot)))
					  (with-output-to-string (s *text*)
					    ,@body)
					  (free-temp-register params)
					  output))))

(defprocedure '+
    (format s "add ~{~a~^,  ~} ~%" (cons output params) ))

(defprocedure '-
    (format s "sub ~{~a~^,  ~} ~%" (cons output params) ))

(defprocedure '*
    (format s "mul ~{~a~^,  ~} ~%" (cons output params) ))

(defprocedure '/
    (format s "div ~{~a~^,  ~} ~%"  params )
  (format s "mflo ~a~%" output))

(defprocedure 'exit
    (format s "li $v0 ,10~%")
  (format s "syscall~%"))

(defprocedure '%
    (format s "div ~{~a~^,  ~} ~%"  params )
  (format s "mfhi ~a~%" output))

(defprocedure 'set
    (format s "move ~{~a~^, ~} ~%" params))

(defprocedure 'read-integer
    (format s "li $v0 , 5 ~%")
  (format s "syscall~%")
  (format s "move ~a , $v0~%" output))

(defprocedure 'print-integer
    (format s "li $v0 , 1~%")
  (format s "move $a0 , ~{~a~}~%" params)
  (format s "syscall~%")
    )
(defprocedure '>=
    (format s "bge ~{~a~^,~}~%" params))

(defprocedure '>
    (format s "bgt ~{~a~^,~}~%" params))

(defprocedure '<=
    (format s "ble ~{~a~^,~}~%" params))

(defprocedure '<
    (format s "blt ~{~a~^,~}~%" params))

(defprocedure '=
    (format s "beq ~{~a~^,~}~%" params))
(defprocedure '!=
    (format s "bneq ~{~a~^,~}~%" params))




(setf (gethash 'print-string *procedures*) (lambda (params)
					     (with-output-to-string (s *text*)
					       (format s "la $a0 , ~a~%" (first params) )
					       (format s "li $v0 , 4~%")
					       (format s "syscall~%"))))

