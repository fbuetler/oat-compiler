	.file	"knut.c"
	.text
	.type	b.2318, @function
b.2318:
.LFB2:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%r10, %rax
	movq	%r10, -8(%rbp)
	movl	40(%rax), %edx
	subl	$1, %edx
	movl	%edx, 40(%rax)
	movl	40(%rax), %edi
	movq	32(%rax), %rdx
	movq	24(%rax), %rcx
	movq	16(%rax), %r8
	movq	8(%rax), %r9
	leaq	44(%rax), %rsi
	movq	(%rax), %rax
	movq	%rax, %r10
	call	a.2316
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2:
	.size	b.2318, .-b.2318
	.type	a.2316, @function
a.2316:
.LFB1:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$168, %rsp
	.cfi_offset 3, -24
	movl	%edi, -116(%rbp)
	movq	%rsi, -128(%rbp)
	movq	%rdx, -136(%rbp)
	movq	%rcx, -144(%rbp)
	movq	%r8, -152(%rbp)
	movq	%r9, -160(%rbp)
	movq	%r10, %rdx
	movq	%r10, -168(%rbp)
	movq	%fs:40, %rax
	movq	%rax, -24(%rbp)
	xorl	%eax, %eax
	leaq	16(%rbp), %rax
	movq	%rax, -40(%rbp)
	movl	-116(%rbp), %eax
	movl	%eax, -72(%rbp)
	movq	-128(%rbp), %rax
	movq	%rax, -80(%rbp)
	movq	-136(%rbp), %rax
	movq	%rax, -88(%rbp)
	movq	-144(%rbp), %rax
	movq	%rax, -96(%rbp)
	movq	-152(%rbp), %rax
	movq	%rax, -104(%rbp)
	movq	%rdx, -112(%rbp)
	leaq	-112(%rbp), %rax
	addq	$44, %rax
	leaq	-112(%rbp), %rdx
	movw	$-17591, (%rax)
	leaq	b.2318(%rip), %rcx
	movq	%rcx, 2(%rax)
	movw	$-17847, 10(%rax)
	movq	%rdx, 12(%rax)
	movl	$-1864106167, 20(%rax)
	movl	-72(%rbp), %eax
	testl	%eax, %eax
	jg	.L4
	movq	-104(%rbp), %rdx
	movl	$0, %eax
	call	*%rdx
	movl	%eax, %ebx
	movq	-160(%rbp), %rdx
	movl	$0, %eax
	call	*%rdx
	addl	%ebx, %eax
	jmp	.L6
.L4:
	leaq	-112(%rbp), %rax
	movq	%rax, %r10
	movl	$0, %eax
	call	b.2318
.L6:
	movq	-24(%rbp), %rsi
	xorq	%fs:40, %rsi
	je	.L7
	call	__stack_chk_fail@PLT
.L7:
	addq	$168, %rsp
	popq	%rbx
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	a.2316, .-a.2316
	.section	.rodata
.LC0:
	.string	" %d\n"
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
	subq	$160, %rsp
	movq	%fs:40, %rax
	movq	%rax, -8(%rbp)
	xorl	%eax, %eax
	leaq	16(%rbp), %rax
	movq	%rax, -16(%rbp)
	leaq	-160(%rbp), %rax
	leaq	-160(%rbp), %rdx
	movw	$-17591, (%rax)
	leaq	lambda.2320(%rip), %rcx
	movq	%rcx, 2(%rax)
	movw	$-17847, 10(%rax)
	movq	%rdx, 12(%rax)
	movl	$-1864106167, 20(%rax)
	leaq	-160(%rbp), %rax
	addq	$28, %rax
	leaq	-160(%rbp), %rdx
	movw	$-17591, (%rax)
	leaq	lambda.2323(%rip), %rcx
	movq	%rcx, 2(%rax)
	movw	$-17847, 10(%rax)
	movq	%rdx, 12(%rax)
	movl	$-1864106167, 20(%rax)
	leaq	-160(%rbp), %rax
	addq	$56, %rax
	leaq	-160(%rbp), %rdx
	movw	$-17591, (%rax)
	leaq	lambda.2326(%rip), %rcx
	movq	%rcx, 2(%rax)
	movw	$-17847, 10(%rax)
	movq	%rdx, 12(%rax)
	movl	$-1864106167, 20(%rax)
	leaq	-160(%rbp), %rax
	addq	$84, %rax
	leaq	-160(%rbp), %rdx
	movw	$-17591, (%rax)
	leaq	lambda.2329(%rip), %rcx
	movq	%rcx, 2(%rax)
	movw	$-17847, 10(%rax)
	movq	%rdx, 12(%rax)
	movl	$-1864106167, 20(%rax)
	leaq	-160(%rbp), %rax
	addq	$112, %rax
	leaq	-160(%rbp), %rdx
	movw	$-17591, (%rax)
	leaq	lambda.2332(%rip), %rcx
	movq	%rcx, 2(%rax)
	movw	$-17847, 10(%rax)
	movq	%rdx, 12(%rax)
	movl	$-1864106167, 20(%rax)
	leaq	-160(%rbp), %rax
	addq	$112, %rax
	movq	%rax, %r8
	leaq	-160(%rbp), %rax
	addq	$84, %rax
	movq	%rax, %rdi
	leaq	-160(%rbp), %rax
	addq	$56, %rax
	movq	%rax, %rcx
	leaq	-160(%rbp), %rax
	addq	$28, %rax
	movq	%rax, %rdx
	leaq	-160(%rbp), %rax
	movq	%rax, %rsi
	leaq	-160(%rbp), %rax
	movq	%rax, %r10
	movq	%r8, %r9
	movq	%rdi, %r8
	movl	$10, %edi
	call	a.2316
	movl	%eax, %esi
	leaq	.LC0(%rip), %rdi
	movl	$0, %eax
	call	printf@PLT
	movl	$0, %eax
	movq	-8(%rbp), %rsi
	xorq	%fs:40, %rsi
	je	.L10
	call	__stack_chk_fail@PLT
.L10:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	main, .-main
	.type	lambda.2320, @function
lambda.2320:
.LFB3:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%r10, -8(%rbp)
	movl	$1, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE3:
	.size	lambda.2320, .-lambda.2320
	.type	lambda.2323, @function
lambda.2323:
.LFB4:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%r10, -8(%rbp)
	movl	$-1, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE4:
	.size	lambda.2323, .-lambda.2323
	.type	lambda.2326, @function
lambda.2326:
.LFB5:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%r10, -8(%rbp)
	movl	$-1, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE5:
	.size	lambda.2326, .-lambda.2326
	.type	lambda.2329, @function
lambda.2329:
.LFB6:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%r10, -8(%rbp)
	movl	$1, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE6:
	.size	lambda.2329, .-lambda.2329
	.type	lambda.2332, @function
lambda.2332:
.LFB7:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%r10, -8(%rbp)
	movl	$0, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE7:
	.size	lambda.2332, .-lambda.2332
	.ident	"GCC: (GNU) 9.1.0"
	.section	.note.GNU-stack,"x",@progbits
