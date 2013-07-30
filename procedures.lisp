(defparameter *procedures* (make-hash-table))

(defmacro defprocedure (func  output &body body)
  "make primitive procedures"
  (if output
      (setf output '(temp-register-allot))
      (setf output nil))
  `(setf (gethash ,func *procedures*) (lambda (params)
					(let ((output ,output))
					  (with-output-to-string (s *text*)
					    ,@body)
					  (free-temp-register params)
					  output))))

(defprocedure '+ t
    (format s "add ~{~a~^,  ~} ~%" (cons output params) ))

(defprocedure '- t
    (format s "sub ~{~a~^,  ~} ~%" (cons output params) ))

(defprocedure '* t
    (format s "mul ~{~a~^,  ~} ~%" (cons output params) ))

(defprocedure '/ t
    (format s "div ~{~a~^,  ~} ~%"  params )
  (format s "mflo ~a~%" output))

(defprocedure 'exit nil
    (format s "li $v0 ,10~%")
  (format s "syscall~%"))

(defprocedure '% t
    (format s "div ~{~a~^,  ~} ~%"  params )
  (format s "mfhi ~a~%" output))

(defprocedure 'set nil 
    (format s "move ~{~a~^, ~} ~%" params))

(defprocedure 'read-integer t
    (format s "li $v0 , 5 ~%")
  (format s "syscall~%")
  (format s "move ~a , $v0~%" output))

(defprocedure 'print-integer nil
    (format s "li $v0 , 1~%")
  (format s "move $a0 , ~{~a~}~%" params)
  (format s "syscall~%")
    )
(defprocedure '>= nil
    (format s "bge ~{~a~^,~}~%" params))

(defprocedure '> nil
    (format s "bgt ~{~a~^,~}~%" params))

(defprocedure '<= nil
    (format s "ble ~{~a~^,~}~%" params))

(defprocedure '< nil
    (format s "blt ~{~a~^,~}~%" params))

(defprocedure '= nil
    (format s "beq ~{~a~^,~}~%" params))
(defprocedure '!= nil
    (format s "bneq ~{~a~^,~}~%" params))




(setf (gethash 'print-string *procedures*) (lambda (params)
					     (with-output-to-string (s *text*)
					       (format s "la $a0 , ~a~%" (first params) )
					       (format s "li $v0 , 4~%")
					       (format s "syscall~%"))))

