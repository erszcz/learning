	.text
	.global	_start
_start:
	pushq	%rbp
	movq	%rsp, %rbp

	jmp	1f

	# exit (5)
1:	mov	$5, %rdi
	jmp	exit
