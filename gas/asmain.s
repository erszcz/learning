	.extern cmain

	.text
	.global	main
main:
	pushq	%rbp
	movq	%rsp, %rbp

	# printf (msg00)
	movq	$msg00, %rdi
	call	printf

	call	cmain

	# printf (msg01)
	movq	$msg01, %rdi
	call	printf

	# exit (5)
	mov	$5, %rdi
	jmp	exit

	.data
msg00:	.asciz	"Entry point in asm-space.\n"
msg01:  .asciz	"asm-space: exit(5) (see yourself: echo $?)\n"
