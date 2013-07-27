;;Setting up stuff with all the registers and shit
(defparameter *number-registers* 12)

;;contains name of the register, consed with a number if subscript goes from 0 - n-1
(defparameter *mnemonics*
  '((zero)
    ($at)
    ($v . 2)
    ($a . 4)
    ($t . 8)
    ($s . 8)
    ))

;;Struct to store register data
(defstruct (register ( :print-function (lambda (struct stream depth)
					 (declare (ignore depth))
					 (format stream "~a" (register-name struct)))))
  name value type (temp? nil) (free? t))

;;List storing all the registers
(defvar *register-list* '())

;Creates a list with all the registers
(dolist  (part *mnemonics*)
  (if (cdr part)
      (dotimes (x (cdr part))
	(push (make-register :name (concatenate 'string
						(symbol-name (car part))
						(write-to-string x) ) :type (car part) )
	      *register-list* ))
      (push  (make-register :name (symbol-name (car part)))
	     *register-list*)))
(setf *register-list* (reverse *register-list*))
;(format t "~%All registers loaded~%")

(defun get-trap-code (function)
  (let ((trap-code 0)
	(syscall (list 'print_int 
			 'print_float
			 'print_double 
			 'print_string 
			 'read_int 
			 'read_float 
			 'read_double 
			 'read_string 
			 'sbrk 
			 'exit 
			 'print_char 
			 'read_char
			 'file_open
			 'file_read 
			 'file_write 
			 'file_close )))
    (dolist (sys syscall trap-code)
      (incf trap-code)
      (when (equal function sys)
	(return trap-code))
      )))

;;Environment has frames with variables in it
;; Let allows lexical scoping which will be helpful in environements
;;Can be rewritten using find

(defun find-empty-t-register ()
  (dolist (reg *register-list*)
    (when (and (equal (register-type reg) '$t)
	       (register-free? reg))
      (return reg))))

(defun temp-register-allot (constant)
  (let ((a  (find-empty-t-register)))
    (with-output-to-string (stream *output*)
      (format stream "li ~a , ~a ~%" a constant))
    (setf (register-value a) constant
	  (register-temp? a) t
	  (register-free? a) nil)
    a))

(defun free-temp-register (params)
  "Takes a list of registers and frees those who are temporary"
  (dolist (param params)
    (when (and param (register-temp? param))
      (setf (register-free? param) t))))

(defparameter *global-environment* '())

(defun make-frame (var-val-list)
  "var-val list is ((var value) (var value)). Value defaults to 0"
  (let ((frame (make-hash-table )))
    (dolist (var-val var-val-list)
      (if (listp var-val)
	  (setf (gethash (car var-val) frame) (cdr var-val) )
	  (setf (gethash var-val frame) 0)))))


(defparameter *procedures* (make-hash-table))

(defmacro defprocedure (func &body body)
`(setf (gethash ,func *procedures*) (lambda (params)
				  (let ((output ( find-empty-t-register)))
				    (with-output-to-string (s *output*)
				      ,@body)
				    (free-temp-register params)
				    output)))
  )

(defprocedure '+
  (format s "add ~{~a~^,  ~} ~%" (cons output params) ))

(defprocedure '-
  (format s "sub ~{~a~^,  ~} ~%" (cons output params) ))

(defprocedure '*
  (format s "mul ~{~a~^,  ~} ~%" (cons output params) ))

(defprocedure '/
  (format s "div ~{~a~^,  ~} ~%"  params )
     (format s "mflo ~a~%" output))

(defprocedure '%
  (format s "div ~{~a~^,  ~} ~%"  params )
  (format s "mfhi ~a~%") output)


(defun operator (expr)
  (first expr))

(defun operands (expr)
  (rest expr))


(defun primitive-procedure? (sym)
  (member sym *primitive-procedures*))



(defun evaluate (expr env)
  (cond ((numberp expr) (temp-register-allot expr) )
	((register-p expr) expr)
	((listp expr)
	 (apply! (operator expr) (list-of-values (operands expr) env)))))


(defun apply! (funct params)
  "Takes a fuction and its parameters and converts to asm"
  (funcall (gethash funct *procedures*) params)
 )

(defun list-of-values (operands env)
  (if (not operands)
      '()
      (cons
       (evaluate (first operands) env)
       (list-of-values (rest operands) env))))

;(format t "Given arguments are ~&~S~&" *args*)
(defparameter *output* (make-array 0 
                :element-type 'character 
                :adjustable t 
                :fill-pointer 0))
	

;;Parser
;;parses into a list, PERFECT!
(defun get-file (filename)
  (with-open-file (stream filename)
    (read stream)))


(unless (= 2 (length *args*))
  (error "Incorrect arguments given. Should be of form: %prog input-file output-file"))
(defparameter *code* (get-file (first *args*)))


(evaluate *code* '())
(let ((stream (open (second *args*) :direction :output)))
  (format stream "~a" *output*)
  (format t "~a" *output*)
  (close stream))
