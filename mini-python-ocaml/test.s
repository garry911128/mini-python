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
	subq $24, %rsp
	movq $8, %rdi
	call malloc_wrapper
	movabsq $str0, %rax
	movq %rax, -8(%rbp)
	movq $8, %rdi
	call malloc_wrapper
	movabsq $str1, %rax
	movq %rax, -16(%rbp)
	movq $8, %rdi
	call malloc_wrapper
	movabsq $str2, %rax
	movq %rax, -24(%rbp)
#print_str
	movq -24(%rbp), %rax
	movq %rax, %rsi
	leaq print_str, %rdi
	call printf_wrapper
#print_str
	movq -8(%rbp), %rax
	movq %rax, %rsi
	leaq print_str, %rdi
	call printf_wrapper
	addq $24, %rsp
	xorq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	.data
str0:
	.string "hello"
str1:
	.string "he"
str2:
	.string "llo"
print_int:
	.string "%d\n"
print_str:
	.string "%s\n"
