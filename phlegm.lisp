(load "auxfns.lisp")


(dbug  :input :output)
;;debug ids are :code :output :register :eval :env

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

;;Creates a list with all the registers
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

;;TODO Can be rewritten using find

(defun find-empty-t-register ()
  "find an empty register of type t"
  (dolist (reg *register-list*)
    (when (and (equal (register-type reg) '$t)
	       (register-free? reg))
      (return reg))))

(defun temp-register-allot (constant)
  "Temporarily allot a t register"
  (let ((a  (find-empty-t-register)))
    (with-output-to-string (stream *text*)
      (format stream "li ~a , ~a ~%" a constant))
    (setf (register-value a) constant
	  (register-temp? a) t
	  (register-free? a) nil)
    a))


(defun var-register-allot (constant)
  "Allot a t register to a variable"
  (let ((a  (find-empty-t-register)))
    (with-output-to-string (stream *text*)
      (format stream "li ~a , ~a ~%" a constant))
    (setf (register-value a) constant
	  (register-temp? a) nil
	  (register-free? a) nil)
    a))

;; TODO: Convert into a macro for freeing register

(defun free-temp-register (params)
  "Takes a list of registers and frees those which are temporary"
  (dolist (param params)
    (when (and param
	       (register-p param)
	       (register-temp? param))
      (setf (register-free? param) t))))


(defun register-status ()
  (let ((register-list '()))
    (dolist (register *register-list*)
      (setf register-list (append (list (register-name register) 'TEMP (register-temp? register) 'FREE? (register-free? register )) register-list))
      )
    register-list))

(register-status)

(defun free-register-list (params)
  "Takes a list of registers and frees those which are temporary"
  (dolist (param params)
    (setf (register-free? param) t)))

;;Environment has frames with variables in it

(defparameter *env* '())

(defun make-frame (var-val-list)
  "var-val list is ((var value) (var value)). Value defaults to 0"
  (let ((frame '()))
    (dolist (var-val var-val-list)
      (if (listp var-val)
	  (setf frame (acons (first var-val) (var-register-allot (second var-val))  frame))
	  (setf frame (acons  var-val (var-register-allot 0)  frame ))))
    (dbg :frame "Frame to be added ~a ~%" frame)
    frame))

(defun add-frame (var-val-list)
  (dbg :frame "Adding frame with vars ~a ~%" var-val-list)
  (push (make-frame var-val-list) *env*))

(defun pop-frame ()
  (let ((register-list '()))
    (dolist ( var (first *env*))
      (push (cdr var) register-list))
    (free-register-list register-list)))

(defparameter *procedures* (make-hash-table))

(defmacro defprocedure (func &body body)
  "make primitive procedures"
  `(setf (gethash ,func *procedures*) (lambda (params)
					(let ((output ( find-empty-t-register)))
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


(defun get-trap-code (function)
  "Returns the trap code for syscalls"
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
	(return trap-code)))))

(defun operator (expr)
  "returns operator of expression"
  (first expr))

(defun operands (expr)
  "Returns operands of expressions"
  (rest expr))

(defun variablep (expr)
  "Find if the variable exists in the environment "
  (dolist (frame *env*)
    (when (assoc expr frame)
      (return t))))

(defun get-register-for-var (var)
  (dolist (frame *env*)
    (when (assoc var frame)
      (return (cdr (assoc var frame))))
    ))


(defun letp (expr)
  (equal (car expr) 'let))


(defun ifp (expr)
  (equal (car expr) 'if))

(defun multi-exprp (exprs)
  (if (listp exprs)
      (every #'listp exprs)
      nil))
(defun whilep (expr)
  (equal (car expr) 'while))

(defmacro write-to-output (stream output &body body)
  "Stream is this arbitary name for stream used in format , output is *data* or *text*"
  `(with-output-to-string (,stream ,output)
	,@body))


;;Returning string-name as a string. Be careful.
(let ((no-string 1))
  (defun get-string-name (expr)
    (let (( string-name (concatenate 'string "string" (write-to-string no-string))))
      (incf no-string)
      (with-output-to-string (s *data*)
	(format s "~a: ~%" string-name )
	(format s ".asciiz \"~a\"~%" expr)
	string-name
	))))

;;;TESTING


(defmacro get-*-name (sym)
  (let* ((helper-name (intern (format nil "get-~a-name" sym))))
    `(let ((count 1))
       (defun ,helper-name ()
	 (let (( string-name (concatenate 'string ,(symbol-name sym) (write-to-string count))))
	   (incf count)
	   string-name)))))
;;;TESTING

(let ((count 1))
  (defun get-if-name ()
    (let (( string-name (concatenate 'string "if" (write-to-string count))))
      (incf count)
      string-name)))

(let ((count 1))
  (defun get-else-name ()
    (let (( string-name (concatenate 'string "else" (write-to-string count))))
      (incf count)
      string-name)))


(let ((count 1))
  (defun get-while-name ()
    (let (( string-name (concatenate 'string "while" (write-to-string count))))
      (incf count)
      string-name)))

(let ((count 1))
  (defun get-while-exit-name ()
    (let (( string-name (concatenate 'string "while_exit" (write-to-string count))))
      (incf count)
      string-name)))



(let ((count 1))
  (defun get-while-loop-name ()
    (let (( string-name (concatenate 'string "while_loop" (write-to-string count))))
      (incf count)
      string-name)))

(let ((count 1))
  (defun get-cont-name ()
    (let (( string-name (concatenate 'string "cont" (write-to-string count))))
      (incf count)
      string-name)))
 

(defun handle-if (expr)
  (let ((c (make-label :name (get-cont-name)))
	(if-name (make-label :name (get-if-name)))
	(else-name (make-label :name (get-else-name))))
    
    (evaluate (append (second expr) (list if-name)))
    (write-to-output stream *text*
      (format stream "b ~a~%" else-name)
      (format stream "~a:~%" if-name))
    (evaluate (third expr))
    (write-to-output stream *text*
      (format stream "b ~a~%~a:~%" c  else-name))
    (evaluate (fourth expr))
    (write-to-output stream *text*
      (format stream "b ~a~%" c )
      (format stream "~a:~%" c))))

(defun handle-while (expr)
  (let ((while- (make-label :name (get-while-name)))
	(while-loop (make-label :name (get-while-loop-name)))
	(while-exit (make-label :name (get-while-exit-name))))
    (write-to-output stream *text*
      (format stream "~a:~%" while-)
      (evaluate  (append (second expr) (list  while-loop)))
      (format stream "b ~a~%" while-exit)
      (format stream "~a:~%" while-loop)
      (evaluate (cddr expr))
      (format stream "b ~a~%" while- )
      (format stream "~a:~%" while-exit))))

(defstruct (label ( :print-function (lambda (struct stream depth)
					 (declare (ignore depth))
					 (format stream "~a" (label-name struct)))))
  name)


					;Write a better eval that takes into acco
(defun evaluate (expr)
  (dbg :register "Current list of registers is ~{~a ~^,~}~%" (register-status))
  (dbg :eval "Evaluating: ~a~%" expr)
  (dbg :env "The current environmnet is ~a ~%" *env*)
  (cond ((multi-exprp expr) 
	 (dolist (exp expr)
	   (evaluate exp)))
	((label-p expr) )
	((variablep expr))
	
	((numberp expr) (temp-register-allot expr))
	((stringp expr) (get-string-name expr))
	((register-p expr)  expr )
	((ifp expr) (handle-if expr))
	((whilep expr) (handle-while expr))
	
	((letp expr)
	
	 (add-frame (cadr expr))
	 (evaluate (cddr expr) )
	 (pop-frame)) 
	((listp expr)
	 (apply! (operator expr) (list-of-values (operands expr))))))

(defun apply! (funct params)
  "Takes a fuction and its parameters and converts to asm"
  (funcall (gethash funct *procedures*) params)
  )

(defun list-of-values (operands)
  "Evaluates the operands recursively"
  (if (not operands)
      '()
      (cons
       (evaluate (first operands))
       
       (list-of-values (rest operands)))))


;;Contains the .text of SPIM
(defparameter *text* (make-array 0 
				 :element-type 'character 
				 :adjustable t 
				 :fill-pointer 0))

;;Contains the .data of SPIM
(defparameter *data* (make-array 0 
				 :element-type 'character 
				 :adjustable t 
				 :fill-pointer 0))



;;Parser
;;parses into a lists
(defun get-file (filename)
  (let (( sexps '()))
    (with-open-file (stream filename)
      (do ((line (read stream nil 'eof)
		 (read stream nil 'eof)))
	  ((eql line 'eof))
	(push line sexps)))
    (reverse sexps)))

;;Checks if both arguments are present
(unless (= 2 (length *args*))
  (error "Incorrect arguments given. Should be of form: %prog input-file output-file"))

(defparameter *code* (get-file (first *args*)))

;;Drools the mucus
(dbg :input "INPUT: ~% ~a~%" *code*)


(defparameter *output* (concatenate 'string
				    (format nil ".data~%")
				    (format nil "~a" *data*)
				    (format nil ".text~%.globl main~%main:~%")
				    (format nil "~a" *text*)))

;;Here is where it all starts
(evaluate *code*)

;;Print out the compiled code to standard output and file
(let ((stream (open (second *args*) :direction :output)))
 
  (format stream ".data~%")
  (format stream "~(~a~)" *data*)
  (format stream ".text~%.globl main~%main:~%")
  (format stream "~(~a~)" *text*)
  (dbg :output "~%~%OUTPUT: ~%")
  (dbg :output ".data~%")
  (dbg :output "~(~a~)" *data*)
  (dbg :output ".text~%.globl main~%main:~%")
  (dbg :output "~(~a~)" *text*)
  (close stream))



