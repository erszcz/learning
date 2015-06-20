	.text
	.global	main
main:
	pushq	%rbp
	movq	%rsp, %rbp

	movl	$2,%eax
	testl	%eax,%eax
	jz	1
	mov	$1,%rdi
	jmp	exit

1:	mov	$0,%rdi
	jmp	exit
