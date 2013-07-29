# Phlegm

Coughs SPIM

```bash
$ clisp phlegm.lisp test.phlm output.asm
```


## Example

###Input
```lisp
(let (sum)
  (print-string "Hello")
  (set sum (read-integer)))
```

###Output
Output

```asm
.data
string1: 
.asciiz "hello"
.text
.globl main
main:
li $t0 , 0 
la $a0 , string1
li $v0 , 4
syscall
li $v0 , 5 
syscall
move $t1 , $v0
move $t0, $t1 
li $t0 , 1 
li $t1 , 2 
add $t2,  $t0,  $t1 
```
