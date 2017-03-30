#MILAN SINGH 
.data
bitmapDisplay: .space 0x80000 # enough memory for a 512x256 bitmap display
resolution: .word  512 256    # width and height of the bitmap display
newLine: .asciiz "\n"
imagI: .asciiz "i"
plus: .asciiz " + "
x: .asciiz "x"
plusY: .asciiz " + y"
iColon: .asciiz " i : "

windowlrbt: 
.float -2.5 2.5 -1.25 1.25  					# good window for viewing Julia sets
#.float -3 2 -1.25 1.25  					# good window for viewing full Mandelbrot set
#.float -0.807298 -0.799298 -0.179996 -0.175996 		# double spiral
#.float -1.019741354 -1.013877846  -0.325120847 -0.322189093 	# baby Mandelbrot
 
bound: .float 100	# bound for testing for unbounded growth during iteration
maxIter: .word 16	# maximum iteration count to be used by drawJulia and drawMandelbrot
scale: .word 48		# scale parameter used by computeColour

# Julia constants for testing, or likewise for more examples see
# https://en.wikipedia.org/wiki/Julia_set#Quadratic_polynomials  
JuliaC0:  .float 0.0    0    # should give you a circle, a good test, though boring!
JuliaC1:  .float 0.25 0.5 
JuliaC2:  .float -0.3132	 -0.5    #0    0.7 
JuliaC3:  .float -0.6    0.0

# a demo starting point for iteration tests
z0: .float  0 0

########################################################################################
.text
	
	
	###TEST ITERATE VERBOSE###
	#la $t0, JuliaC1		#Loads address of JuliaC1
	#li $a0, 10		#arg N = 10
	#l.s $f12, ($t0) 	#load 0.25 -> arg a
	#l.s $f13, 4($t0)	#load 0.5 -> arg b
	#la $t0, JuliaC0		#Loads address of JuliaC0 into t1
	#l.s $f14, ($t0) 	#load 0.0 -> arg x0
	#l.s $f15, 4($t0)	#load 0.0 -> arg y0
	#jal iterateVerbose	#testin iterateVerbosela $t0, JuliaC1		#Loads address of JuliaC1
	
	###TEST ITERATE###
	#la $t0, JuliaC1
	#li $a0, 10		#arg N = 10
	#l.s $f12, ($t0) 	#load 0.25 -> arg a
	#l.s $f13, 4($t0)	#load 0.5 -> arg b
	#la $t0, JuliaC0		#Loads address of JuliaC0 into t1
	#l.s $f14, ($t0) 	#load 0.0 -> arg x0
	#l.s $f15, 4($t0)	#load 0.0 -> arg y0
	#jal iterate		#testin iterate	
	#add $a0, $0, $v0
	#add $v0, $0, 1
	#syscall
	
	###TEST pixel2Complex###
	#li $a0, 512
	#li $a1, 256
	#jal pixel2ComplexInWindow
	#mtc1 $0, $f4
	#add.s $f12, $f0, $f4
	#add.s $f13, $f1, $f4
	#jal printComplex
	
	###drawJulia
	la $t0, JuliaC2
	l.s $f12, ($t0)
	l.s $f13, 4($t0)
	jal drawJulia #Comment this and uncomment line 77 for Mandelbrot
	
	###drawMandelbrot
	#jal drawMandelbrot
	
	li $v0 10 # exit
	syscall


# TODO: Write your functions to implement various assignment objectives here


########################################################################################
# Computes a colour corresponding to a given iteration count in $a0
# The colours cycle smoothly through green blue and red, with a speed adjustable 
# by a scale parametre defined in the static .data segment
computeColour:
	la $t0 scale
	lw $t0 ($t0)
	mult $a0 $t0
	mflo $a0
ccLoop:
	slti $t0 $a0 256
	beq $t0 $0 ccSkip1
	li $t1 255
	sub $t1 $t1 $a0
	sll $t1 $t1 8
	add $v0 $t1 $a0
	jr $ra
ccSkip1:
  	slti $t0 $a0 512
	beq $t0 $0 ccSkip2
	addi $v0 $a0 -256
	li $t1 255
	sub $t1 $t1 $v0
	sll $v0 $v0 16
	or $v0 $v0 $t1
	jr $ra
ccSkip2:
	slti $t0 $a0 768
	beq $t0 $0 ccSkip3
	addi $v0 $a0 -512
	li $t1 255
	sub $t1 $t1 $v0
	sll $t1 $t1 16
	sll $v0 $v0 8
	or $v0 $v0 $t1
	jr $ra
ccSkip3:
 	addi $a0 $a0 -768
 	j ccLoop

printNewLine:
	li $v0, 4		#Loads print string for syscall
	la $a0, newLine		#loads newline into arg for syscall
	syscall			#prints
	jr $ra			#returns to calling code
	
printComplex:
	li $v0, 2 		#Loads print float for syscall
	syscall			#prints first arguments (stored in $f12 when printComplex is called
	li $v0, 4		#Loads print string for syscall
	la $a0, plus		#loads " + " into argument for syscall
	syscall			#prints
	li $v0, 2 		#Loads print float for syscall
	mtc1 $0,$f4 		#Sets temp f4 to zero
	add.s $f12, $f4, $f13	#Loads second passed arg into $f12 for print
	syscall			#prints
	li $v0, 4 		#Loads print string for syscall
	la $a0, imagI		#loads "i" into argument for syscall
	syscall			#prints
	addi $sp, $sp, -4	#update sp
	sw $ra, 0($sp)		#saves $ra
	jal printNewLine	#prints a newline
	lw $ra, 0($sp)		#restore $ra
	addi $sp, $sp, 4	#restore sp
	jr $ra			#returns to calling code
printXY:
	add $t1, $a0, $0
	li $v0, 4
	la $a0, x
	syscall
	li $v0, 1 
	add $a0, $t1, $0
	syscall
	li $v0, 4
	la $a0, plusY
	syscall
	li $v0, 1
	add $a0, $t1, $0
	syscall
	li $v0, 4
	la $a0, iColon
	syscall
	jr $ra	
multComplex:			#compute (a+bi)(c+di) = (ac-db)+(ad+bc)i
	mul.s $f4, $f12, $f14 	#$f4 = a*c
	mul.s $f6, $f13, $f15	#$f6 = b*d
	sub.s $f0, $f4, $f6	#$f0 = a*c - b*d REAL PART
	mul.s $f4, $f12, $f15 	#$f12 = a*d
	mul.s $f6, $f13, $f14	#$f13 = b*c
	add.s $f1, $f4, $f6	#$f1 = b*c + a*d IMAGINARY PART
	jr $ra
	
iterateVerbose: #to be restored: $ra, $f20, $f21, $f22, $s0, $s1
	addi $sp $sp -24
	sw $ra, ($sp)
	s.s $f20, 4($sp)
	s.s $f21, 8($sp)
	s.s $f22, 12($sp)
	sw $s0, 16($sp)
	sw $s1, 20($sp)
	mtc1 $0, $f22		#$f22 = 0
	addi $s1, $0, 1		#s1 = 0	
	add $s2, $a0, $0	#s2 = N	
      ###Print BEGIN
      	add $a0, $0, $0
	jal printXY
      	add.s $f20, $f12, $f22
      	add.s $f21, $f13, $f22
      	add.s $f12, $f14, $f22
      	add.s $f13, $f15, $f22
	jal printComplex
	add.s $f12, $f20, $f22
      	add.s $f13, $f21, $f22
      ###Print END
vLoop:
     ####Computation START	
      ###Call multComplex BEGIN
	add.s $f20,$f22,$f12 	#f20 = a
	add.s $f21,$f22,$f13	#f21 = b
	add.s $f12, $f22, $f14	#$f12 = x
	add.s $f13, $f22, $f15	#$f13 = y	
	jal multComplex		#f0 = x^2-y^2, f1 = 2xy
      ###Call multComplex END
      ###Shift values BEGIN
	add.s $f12, $f22, $f20	#$f12 = a
	add.s $f13, $f22, $f21	#$f13 = b
	add.s $f14,$f12,$f0	#new x -> x^2-y^2+a
	add.s $f15,$f13,$f1	#new y -> 2xy+b
      ###Shift values END
     ####Computation  END     
     #### bound < x^2 + y^2 START	
      	mul.s $f20, $f14, $f14 	#f20 = x^2
      	mul.s $f21, $f15, $f15	#f22 = y^2
      	add.s $f20, $f20, $f21	#f20 = x^2 + y^2
      	l.s $f21, bound		#load bound value
	c.lt.s $f21, $f20	# if bound < x^2 + y^2 set fp cond flag to true
	bc1t bEndVIter		#conditional jump to gtBound if bound < x^2 + y^2 (does not change $ra)
     #### bound < x^2 + y^2 END
     ####Print BEGIN
      	add $a0, $s1, $0
	jal printXY
      	add.s $f20, $f12, $f22
      	add.s $f21, $f13, $f22
      	add.s $f12, $f14, $f22
      	add.s $f13, $f15, $f22	
	jal printComplex
	add.s $f12, $f20, $f22
      	add.s $f13, $f21, $f22
     ####Print END
	addi $s1, $s1, 1	#i++
     	slt $t0, $s1, $s2
	beq $t0, $0 endVIter	#if i == n endIter
	j vLoop			#else Loop		
bEndVIter:
	addi $s1, $s1, -1
	j endVIter
endVIter:		
	add $a0, $s1, $0	#a0 = i
	li $v0, 1		#load print int
	syscall			#prints i
	add $v0,$0,$s1
	lw $ra, ($sp)
	l.s $f20, 4($sp)
	l.s $f21, 8($sp)
	l.s $f22, 12($sp)
	lw $s0, 16($sp)
	lw $s1, 20($sp)
	addi $sp $sp 24
	jr $ra

iterate: #to be restored: $ra, $f20, $f21, $f22, $s0, $s1
	addi $sp $sp -24
	sw $ra, ($sp)
	s.s $f20, 4($sp)
	s.s $f21, 8($sp)
	s.s $f22, 12($sp)
	sw $s0, 16($sp)
	sw $s1, 20($sp)
	mtc1 $0, $f22		#$f22 = 0
	addi $s1, $0, 1		#s1 = 0	
	add $s2, $a0, $0	#s2 = N	
iLoop:
     ####Computation START	
      ###Call multComplex BEGIN
	add.s $f20,$f22,$f12 	#f20 = a
	add.s $f21,$f22,$f13	#f21 = b
	add.s $f12, $f22, $f14	#$f12 = x
	add.s $f13, $f22, $f15	#$f13 = y	
	jal multComplex		#f0 = x^2-y^2, f1 = 2xy
      ###Call multComplex END
      ###Shift values BEGIN
	add.s $f12, $f22, $f20	#$f12 = a
	add.s $f13, $f22, $f21	#$f13 = b
	add.s $f14,$f12,$f0	#new x -> x^2-y^2+a
	add.s $f15,$f13,$f1	#new y -> 2xy+b
      ###Shift values END
     ####Computation  END     
     #### bound < x^2 + y^2 START	
      	mul.s $f20, $f14, $f14 	#f20 = x^2
      	mul.s $f21, $f15, $f15	#f22 = y^2
      	add.s $f20, $f20, $f21	#f20 = x^2 + y^2
      	l.s $f21, bound		#load bound value
	c.lt.s $f21, $f20	# if bound < x^2 + y^2 set fp cond flag to true
	bc1t bEndIter		#conditional jump to gtBound if bound < x^2 + y^2 (does not change $ra)
     #### bound < x^2 + y^2 END
	addi $s1, $s1, 1	#i++
     	slt $t0, $s1, $s2
	beq $t0, $0 endIter	#if i == n endIter
	j iLoop			#else Loop		
bEndIter:
	addi $s1, $s1, -1
	j endIter
endIter:		
	add $v0,$0,$s1
	lw $ra, ($sp)
	l.s $f20, 4($sp)
	l.s $f21, 8($sp)
	l.s $f22, 12($sp)
	lw $s0, 16($sp)
	lw $s1, 20($sp)
	addi $sp $sp 24
	jr $ra

pixel2ComplexInWindow:
      ###register preservation
      	addi $sp, $sp, -24
      	s.s $f20, ($sp)
	s.s $f22, 4($sp)
	s.s $f24, 8($sp)
	s.s $f26, 12($sp)
	sw $s0, 16($sp)
	sw $s1, 20($sp)
      ###initialise l,r,b,t = $f20,$f22,$f24,$f26
      	la $t0, windowlrbt
      	l.s $f20, ($t0)		#f20 = l
      	l.s $f22, 4($t0)	#f22 = r
      	l.s $f24, 8($t0)	#f24 = b
      	l.s $f26, 12($t0)	#f26 = t
      ###initialise w,h = $s0, $s1
      	la $t0, resolution
      	lw $s0, 0($t0) #s0 = w
      	lw $s1, 4($t0) #s1 = h   	
      ###Computation x BEGIN	
      	mtc1 $s0, $f4 		#Load w into f4
      	cvt.s.w $f4, $f4 	#convert w into float
      	mtc1 $s1, $f6 		#Load h into f6
      	cvt.s.w $f6, $f6 	#convert h into float    	
      	mtc1 $a0, $f8 		#Load col into f8
      	cvt.s.w $f8, $f8 	#convert col into float     	
      	mtc1 $a1, $f10 		#Load row into f10
      	cvt.s.w $f10, $f10	#convert row into float     	
      	div.s $f0,$f8,$f4	#x = col/w
      	div.s $f1,$f10,$f6	#y = row/h      	
      	sub.s $f22, $f22, $f20	#f22 = r-1
      	sub.s $f26, $f26, $f24	#f26 = t-b     	
      	mul.s $f0, $f0, $f22	#x = (col/w)(r-l)
      	mul.s $f1, $f1, $f26	#y = (row/h)(t-b)     	
      	add.s $f0, $f0, $f20	#x = (col/w)(r-l) + l
      	add.s $f1, $f1, $f24	#y = (row/h)(t-b) + b
      ###Computation x END		
      ###register preservation
      	l.s $f20, ($sp)
	l.s $f22, 4($sp)
	l.s $f24, 8($sp)
	l.s $f26, 12($sp)
	lw $s0, 16($sp)
	lw $s1, 20($sp)
      	addi $sp, $sp, 24  	
      	jr $ra
      	
#drawJulia (float a, float b) [ a + bi ]
drawJulia: #Registers to be preserved: s0, s1, s2, s3, f20, f21, f22
	addi $sp, $sp, -32
	sw $s0, ($sp)
	sw $s1, 4($sp)
	sw $s2, 8($sp)
	sw $s3, 12($sp)
	s.s $f20, 16($sp)
	s.s $f21, 20($sp)
	s.s $f22, 24($sp)
	sw $ra, 28($sp)
	add $s0, $0, $0 	#rInc=0	
	add $s1, $0, $0		#cInc=0
	la $s3, bitmapDisplay	#pointer -> bitMapDisplay 
	mtc1 $0, $f20		#f20 = 0
	add.s $f21, $f12, $f18	#f21 = a
	add.s $f22, $f13, $f18	#f22 = b
drawJuliaLoop:
	lw $s2, maxIter		#s2 = maxIter
      ###CALL p2CWIndow
      	add $a0, $s1, $0	#col = cInc
      	add $a1, $s0, $0	#row = rInc
      	jal pixel2ComplexInWindow #now f0 = x, f1= y
      ###CALL ITERATE####
	add $a0, $s2, $0	#N = maxIter
	add.s $f12, $f20, $f21 	#arg a = a
	add.s $f13, $f20, $f22	#arg b = b
	add.s $f14, $f0, $f20	#x0=x
	add.s $f15, $f1, $f20	#y0=y
	jal iterate		#call iterate
      ###isJulia?
	beq $v0, $s2, julia	#if returned i == maxIter -> julia
	j notJulia		#else -> notJulia
iterJuliaRow:
	addi $s0,$s0,1		#rInc++
	la $t2, resolution	#$t2-> resolution
	lw $t2, 4($t2)		#t2=maxRows
	addi $t2, $t2, -1
	slt $t1, $t2, $s0 	#t1 ? (maxRows < rInc) 1 : 0
	bne $t1, $0, juliaDrawn	#if t1 == 1 j drawn	
	add $s1,$0,$0		#else cInc=0
	j drawJuliaLoop			
iterJuliaColumn:
	la $t2, resolution		#t2->resolution
	lw $t2, 0($t2)			#t2 = col
	addi $t2, $t2, -1
	beq $s1, $t2, iterJuliaRow		#if cInc == col -> iterRow
	addi, $s1,$s1,1			#cInc++
	j drawJuliaLoop			
julia:
	add $v0, $0, $0
	sw $v0, ($s3) 
	addi $s3, $s3, 4
	j iterJuliaColumn
notJulia:
	add $a0, $v0, $0
	jal computeColour
	sw $v0, ($s3)
	addi $s3, $s3, 4
	j iterJuliaColumn
juliaDrawn: 
	lw $s0, ($sp)
	lw $s1, 4($sp)
	lw $s2, 8($sp)
	lw $s3, 12($sp)
	l.s $f20, 16($sp)
	l.s $f21, 20($sp)
	l.s $f22, 24($sp)
	lw $ra, 28($sp)
     	addi $sp, $sp, 32
	jr $ra

drawMandelbrot:
	addi $sp, $sp, -24
	sw $s0, ($sp)
	sw $s1, 4($sp)
	sw $s2, 8($sp)
	sw $s3, 12($sp)
	s.s $f20, 16($sp)
	sw $ra, 20($sp)
	add $s0, $0, $0 	#rInc=0	
	add $s1, $0, $0		#cInc=0
	la $s3, bitmapDisplay	#pointer -> bitMapDisplay 
	mtc1 $0, $f20		#f20 = 0
drawManLoop:
	lw $s2, maxIter		#s2 = maxIter
      ###CALL p2CWIndow
      	add $a0, $s1, $0	#col = cInc
      	add $a1, $s0, $0	#row = rInc
      	jal pixel2ComplexInWindow #now f0 = x, f1= y
      ###CALL ITERATE####
	add $a0, $s2, $0	#N = maxIter
	add.s $f12, $f20, $f0 	#arg a = a
	add.s $f13, $f20, $f1	#arg b = b
	add.s $f14, $f20, $f20	#x0=0
	add.s $f15, $f20, $f20	#y0=0
	jal iterate		#call iterate
      ###isMan?
	beq $v0, $s2, man	#if returned i == maxIter -> julia
	j notMan		#else -> notJulia
iterManRow:
	addi $s0,$s0,1		#rInc++
	la $t2, resolution	#$t2-> resolution
	lw $t2, 4($t2)		#t2=maxRows
	addi $t2, $t2, -1
	slt $t1, $t2, $s0 	#t1 ? (maxRows < rInc) 1 : 0
	bne $t1, $0, manDrawn	#if t1 == 1 j drawn	
	add $s1,$0,$0		#else cInc=0
	j drawManLoop			
iterManColumn:
	la $t2, resolution		#t2->resolution
	lw $t2, 0($t2)			#t2 = col
	addi $t2, $t2, -1
	beq $s1, $t2, iterManRow		#if cInc == col -> iterRow
	addi, $s1,$s1,1			#cInc++
	j drawManLoop			
manDrawn: 
	lw $s0, ($sp)
	lw $s1, 4($sp)
	lw $s2, 8($sp)
	lw $s3, 12($sp)
	l.s $f20, 16($sp)
	lw $ra, 20($sp)
     	addi $sp, $sp, 24
	jr $ra
man:
	add $v0, $0, $0
	sw $v0, ($s3) 
	addi $s3, $s3, 4
	j iterManColumn
notMan:
	add $a0, $v0, $0
	jal computeColour
	sw $v0, ($s3)
	addi $s3, $s3, 4
	j iterManColumn
	
	
	
