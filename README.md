# Phlegm

Coughs MIPS assembly

```bash
$ clisp phlegm.lisp test.phlm output.asm
```


## Example
A program that tests whether a given integer is <a href="en.wikipedia.org/wiki/Perfect_number">perfect</a> or not.

###Input
```lisp
(print-string "Please enter integer" )
(let (var (sum 0))
  (set var (read-integer))
  (let ((i 1))
    (while (< i var)
      (if (= (% var i) 0)
	  (set sum (+ i sum)))
      (set i (+ i 1))))
  (if (= var sum)
      (print-string "Its a perfect string")
      (print-string "Not Perfect")))
(exit)

```

###Output

```asm
  .data
string1: 
	.asciiz "please enter integer"
string2: 
	.asciiz "its a perfect string"
string3: 
	.asciiz "not perfect"
	.text
	.globl main
main:
	la $a0 , string1
	li $v0 , 4
	syscall
	li $t0 , 0 
	li $t1 , 0 
	li $v0 , 5 
	syscall
	move $t2 , $v0
	move $t0, $t2 
	li $t2 , 1 
while1:
	blt $t2,$t0,while_loop1
	b while_exit1
while_loop1:
	div $t0,  $t2 
	mfhi $t3
	li $t4 , 0 
	beq $t3,$t4,if1
	b else1
if1:
	add $t3,  $t2,  $t1 
	move $t1, $t3 
	b cont1
else1:
	b cont1
cont1:
	li $t3 , 1 
	add $t4,  $t2,  $t3 
	move $t2, $t4 
	b while1
while_exit1:
	beq $t0,$t1,if2
	b else2
if2:
	la $a0 , string2
	li $v0 , 4
	syscall
	b cont2
else2:
	la $a0 , string3
	li $v0 , 4
	syscall
	b cont2
cont2:
	li $v0 ,10
	syscall	

```
