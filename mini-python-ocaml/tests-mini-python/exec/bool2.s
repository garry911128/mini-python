	.text
malloc_wrapper:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
#allign rsp to 16 bytes
	call malloc
	testq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
printf_wrapper:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
#allign rsp to 16 bytes
	call printf
	testq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	addq $0, %rsp
#print_int
	movq $8, %rdi
	call malloc_wrapper
	movq $1, 0(%rax)
	movq 0(%rax), %rdi
	pushq %rdi
	movq $8, %rdi
	call malloc_wrapper
	movq $2, 0(%rax)
	popq %rdi
	movq 0(%rax), %rsi
	cmpq %rsi, %rdi
	sete %dil
	movzbq %dil, %rdi
	pushq %rdi
	movq $8, %rdi
	call malloc_wrapper
	popq %rdi
	movq %rdi, 0(%rax)
	movq 0(%rax), %rsi
	leaq print_int, %rdi
	call printf_wrapper
	subq $0, %rsp
	xorq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	.data
print_int:
	.string "%d\n"
print_str:
	.string "%s\n"
