	.file	"ackerman.c"
	.text
	.section	.rodata
.LC0:
	.string	"%d\n"
	.text
	.globl	main
	.type	main, @function
main:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	$1, %esi
	movl	$3, %edi
	movl	$0, %eax
	call	ackermann
	movl	%eax, %esi
	leaq	.LC0(%rip), %rdi
	movl	$0, %eax
	call	printf@PLT
	movl	$0, %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	main, .-main
	.globl	ackermann
	.type	ackermann, @function
ackermann:
.LFB1:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	cmpl	$0, -4(%rbp)
	jne	.L4
	movl	-8(%rbp), %eax
	addl	$1, %eax
	jmp	.L5
.L4:
	cmpl	$0, -4(%rbp)
	jle	.L6
	cmpl	$0, -8(%rbp)
	jne	.L6
	movl	-4(%rbp), %eax
	subl	$1, %eax
	movl	$1, %esi
	movl	%eax, %edi
	call	ackermann
	jmp	.L5
.L6:
	cmpl	$0, -4(%rbp)
	jle	.L7
	cmpl	$0, -8(%rbp)
	jle	.L7
	movl	-8(%rbp), %eax
	leal	-1(%rax), %edx
	movl	-4(%rbp), %eax
	movl	%edx, %esi
	movl	%eax, %edi
	call	ackermann
	movl	%eax, %edx
	movl	-4(%rbp), %eax
	subl	$1, %eax
	movl	%edx, %esi
	movl	%eax, %edi
	call	ackermann
	jmp	.L5
.L7:
	movl	$-1, %eax
.L5:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	ackermann, .-ackermann
	.ident	"GCC: (GNU) 9.1.0"
	.section	.note.GNU-stack,"",@progbits
