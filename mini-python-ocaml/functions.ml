open Environment
open X86_64
open Utils
open Const


let func_print_list_save_reg : X86_64.text =
  pushq !%rsi ++
  pushq !%rdi ++
  pushq !%rcx ++
  pushq !%r8

let func_print_list_restore_reg : X86_64.text =
  popq r8 ++
  popq rcx ++
  popq rdi ++
  popq rsi



let copy_value_store_reg : X86_64.text =
  pushq !%rcx ++
  pushq !%rax ++
  pushq !%rdi ++
  pushq !%rsi ++
  pushq !%rdx ++
  pushq !%r9

let copy_value_restore_reg : X86_64.text =
  popq r9 ++
  popq rdx ++
  popq rsi ++
  popq rdi ++
  popq rax ++
  popq rcx 


let func_list_save_reg : X86_64.text =
  pushq !%r8 ++
  pushq !%rsi ++
  pushq !%rcx ++
  pushq !%rax

let func_list_restore_reg : X86_64.text =
  popq rax ++
  popq rcx ++
  popq rsi ++
  popq r8
let func env : X86_64.text * X86_64.data = 

  let print_none = unique_label env "print_none" in
  let print_bool = unique_label env "print_bool" in
  let print_bool_false = unique_label env "print_bool_false" in
  let print_bool_end = unique_label env "print_bool_end" in
  let print_int = unique_label env "print_int" in
  let print_string = unique_label env "print_string" in
  let print_string_end = unique_label env "print_string_end" in
  let print_string_in_list = unique_label env "print_string_in_list" in
  let print_string_in_list_2 = unique_label env "print_string_in_list_2" in
  let print_list = unique_label env "print_list" in
  let print_list_end = unique_label env "print_list_end" in
  let print_list_first_time = unique_label env "print_list_first_time" in
  let print_value_end = unique_label env "print_value_end" in

  let copy_two_byte = unique_label env "copy_two_byte" in
  let copy_three_byte = unique_label env "copy_three_byte" in
  let copy_end = unique_label env "copy_end" in
  let copy_list_loop = unique_label env "copy_list_loop" in

  let func_list_concat_next_list = unique_label env "func_list_concat_next_list" in
  let func_list_concat_first_loop = unique_label env "func_list_concat_first_loop" in
  let func_list_concat_second_loop = unique_label env "func_list_concat_second_loop" in
  let func_list_concat_end = unique_label env "func_list_concat_end" in
  
  let func_len = "func_len" in
  let func_list = "func_list" in
  let func_list_loop = "func_list_loop" in
  let func_list_end = "func_list_end" in
  let runtime_error = "runtime_error" in
  let func_difference_eq = "func_difference_eq" in
  let func_difference_neq = "func_difference_neq" in
  let func_difference_failure = "func_difference_💤" in

  let func_list_get = "func_list_get" in

  let text =
    label "print_value" ++
    pushq !%rbp ++
    movq !%rsp !%rbp ++
    movq (ind rdi) !%r10 ++
    cmpq (imm 0) !%r10 ++
    je print_none ++
    cmpq (imm 1) !%r10 ++
    je print_bool ++
    cmpq (imm 2) !%r10 ++
    je print_int ++
    cmpq (imm 3) !%r10 ++
    je print_string ++
    cmpq (imm 4) !%r10 ++
    jne "runtime_error" ++
    movq (imm 1) !%r11 ++
    
    movq (ind ~ofs:(byte) rdi) !%rcx ++
    addq (imm (2 * byte)) !%rdi ++
    movq !%rdi !%rsi ++
    func_print_list_save_reg ++
    put_character '[' ++
    func_print_list_restore_reg ++
    
    label print_list ++
    testq !%rcx !%rcx ++
    jz print_list_end ++
    cmpq !%rdi !%rsi ++
    je print_list_first_time ++
    func_print_list_save_reg ++
    put_character ',' ++
    put_character ' ' ++
    func_print_list_restore_reg ++
    
    label print_list_first_time ++
    func_print_list_save_reg ++
    movq (ind rdi) !%rdi ++
    call "print_value" ++
    func_print_list_restore_reg ++
    decq !%rcx ++
    addq (imm byte) !%rdi ++
    jmp print_list ++
    
    label print_none ++
    leaq (lab "none_string") rdi ++
    xorq !%rax !%rax ++
    call "printf_wrapper" ++
    jmp print_value_end ++

    label print_bool ++
    movq (ind ~ofs:(byte) rdi) !%rsi ++
    cmpq (imm 0) !%rsi ++
    je print_bool_false ++
    leaq (lab "true_string") rdi ++
    jmp print_bool_end ++
    
    label print_bool_false ++
    leaq (lab "false_string") rdi ++
    
    label print_bool_end ++
    xorq !%rax !%rax ++
    call "printf_wrapper" ++
    jmp print_value_end ++

    label print_int ++
    func_print_list_save_reg ++
    movq (ind ~ofs:byte rdi) !%rsi ++
    leaq (lab "format_int") rdi ++
    xorq !%rax !%rax ++
    call "printf_wrapper" ++
    func_print_list_restore_reg ++
    jmp print_value_end ++

    label print_string ++
    cmpq (imm 1) !%r11 ++
    jne print_string_in_list ++
    func_print_list_save_reg ++
    put_character '\'' ++
    func_print_list_restore_reg ++
    
    label print_string_in_list ++
    movq !%rdi !%rax ++
    movq (ind ~ofs:(byte) rax) !%rdi ++
    testq !%rdi !%rdi ++
    jz print_string_end ++
    pushq !%rdi ++
    pushq !%rax ++
    popq rax ++
    popq rdi ++
    movq (ind ~ofs:(2 * byte) rax) !%rdi ++
    xorq !%rax !%rax ++
    call "printf_wrapper" ++
    
    label print_string_end ++
    cmpq (imm 1) !%r11 ++
    jne print_string_in_list_2 ++
    func_print_list_save_reg ++
    put_character '\'' ++
    func_print_list_restore_reg ++
    
    label print_string_in_list_2 ++
    jmp print_value_end ++

    label print_list_end ++
    put_character ']' ++
    
    label print_value_end ++
    leave ++
    ret ++
    
    
    (* 
    rdi: pointer to original value
    rax: return value, pointer to copied value
    *)
    label "copy_two_byte" ++
    pushq !%rdi ++
    movq (imm (2 * byte)) !%rdi ++
    call "malloc_wrapper" ++
    popq rdi ++
    movq (ind rdi) !%r10 ++
    movq !%r10 (ind rax) ++
    movq (ind ~ofs:byte rdi) !%r10 ++
    movq !%r10 (ind ~ofs:byte rax) ++
    ret ++
  
  
    (* 
    rdi: pointer to original value
    rax: return value, pointer to copied value
    *)
    label "copy_string" ++
    pushq !%rdi ++
    movq (imm (3 * byte)) !%rdi ++
    call "malloc_wrapper" ++
    popq rdi ++
    movq (ind rdi) !%r10 ++
    movq !%r10 (ind rax) ++
    movq (ind ~ofs:byte rdi) !%r10 ++
    movq !%r10 (ind ~ofs:byte rax) ++
    movq (ind ~ofs:(2 * byte) rdi) !%r10 ++
    movq !%r10 (ind ~ofs:(2 * byte) rax) ++
    ret ++
    
    
    (* 
    rdi: pointer to original value
    rax: return value, pointer to copied value
    *)
    label "copy_value" ++
    pushq !%rbp ++
    movq !%rsp !%rbp ++
    movq (ind rdi) !%r10 ++
    cmpq (imm 2) !%r10 ++
    jle copy_two_byte ++
    cmpq (imm 3) !%r10 ++
    je copy_three_byte ++
    cmpq (imm 4) !%r10 ++
    jne "runtime_error" ++
    (* 
    rdi: pointer to original value
    rax: return value, pointer to copied value
    *)
    (* malloc new list *)
    pushq !%rdi ++
    movq (ind ~ofs:byte rdi) !%rdi ++
    movq !%rdi !%rcx ++
    addq (imm 2) !%rdi ++
    imulq (imm byte) !%rdi ++
    pushq !%rcx ++
    call "malloc_wrapper" ++
    popq rcx ++
    popq rdi ++
    movq (ind rdi) !%r10 ++
    movq !%r10 (ind rax) ++
    movq !%rcx (ind ~ofs:byte rax) ++
    movq !%rax !%rsi ++
    addq (imm (2 * byte)) !%rsi ++
    addq (imm (2 * byte)) !%rdi ++
    (* 
    rdi : current pointer to elements in original list
    rsi : current pointer to elements in copied list
    rax : pointer to copied list with tag and length copied
    rcx : length of list to be copied
    *)
    label copy_list_loop ++
    testq !%rcx !%rcx ++
    jz copy_end ++
    copy_value_store_reg ++
    movq (ind rdi) !%rdi ++
    call "copy_value" ++
    movq !%rax !%r8 ++
    copy_value_restore_reg ++
    movq !%r8 (ind rsi) ++
    decq !%rcx ++
    addq (imm byte) !%rsi ++
    addq (imm byte) !%rdi ++
    jmp copy_list_loop ++

    label copy_two_byte ++
    call "copy_two_byte" ++
    jmp copy_end ++

    label copy_three_byte ++
    call "copy_string" ++
    jmp copy_end ++

    label copy_end ++
    leave ++
    ret ++
    

    label "concat_list" ++
    pushq !%rbp ++
    movq !%rsp !%rbp ++
    movq (ind rdi) !%r10 ++
    cmpq (imm 4) !%r10 ++
    jne "runtime_error" ++
    movq (ind rsi) !%r10 ++
    cmpq (imm 4) !%r10 ++
    jne "runtime_error" ++
    (* malloc new list *)
    pushq !%rdi ++
    movq (ind ~ofs:byte rdi) !%rdi ++
    pushq !%rsi ++
    movq (ind ~ofs:byte rsi) !%rsi ++
    movq !%rdi !%rcx ++ (* length of first list *)
    movq !%rsi !%r9 ++ (* length of second list *)
    addq !%rsi !%rdi ++
    addq (imm 2) !%rdi ++
    imulq (imm byte) !%rdi ++
    pushq !%rcx ++
    pushq !%r9 ++
    call "malloc_wrapper" ++
    popq r9 ++
    popq rcx ++
    popq rsi ++
    popq rdi ++
    movq (imm 4) (ind rax) ++
    movq !%rcx !%r10 ++
    addq !%r9 !%r10 ++ 
    movq !%r10 (ind ~ofs:byte rax) ++
    addq (imm (2 * byte)) !%rsi ++ 
    pushq !%rsi ++
    movq !%rax !%rdx ++
    addq (imm (2 * byte)) !%rdx ++
    addq (imm (2 * byte)) !%rdi ++
    (* 
    rdi : current pointer to elements in original list
    rdx : current pointer to elements in copied list
    rax : pointer to copied list with tag and length copied
    rcx : length of list to be copied
    *)
    label func_list_concat_first_loop ++
    testq !%rcx !%rcx ++
    jz func_list_concat_next_list ++
    copy_value_store_reg ++
    movq (ind rdi) !%rdi ++
    call "copy_value" ++
    movq !%rax !%r8 ++
    copy_value_restore_reg ++
    movq !%r8 (ind rdx) ++
    decq !%rcx ++
    addq (imm byte) !%rdx ++
    addq (imm byte) !%rdi ++
    jmp func_list_concat_first_loop ++

    label func_list_concat_next_list ++
    movq !%r9 !%rcx ++
    popq rdi ++
    
    label func_list_concat_second_loop ++
    testq !%rcx !%rcx ++
    jz func_list_concat_end ++
    copy_value_store_reg ++
    movq (ind rdi) !%rdi ++
    call "copy_value" ++
    movq !%rax !%r8 ++
    copy_value_restore_reg ++
    movq !%r8 (ind rdx) ++
    decq !%rcx ++
    addq (imm byte) !%rdx ++
    jmp func_list_concat_second_loop ++
    addq (imm byte) !%rdi ++

    label func_list_concat_end ++
    leave ++
    ret ++

    
    label func_len ++
    movq (ind rax) !%r10 ++
    cmpq (imm 2) !%r10 ++
    jle "runtime_error" ++
    movq (ind ~ofs:(byte) rax) !%r8 ++
    movq (imm (2 * byte)) !%rdi ++
    call "malloc_wrapper" ++
    movq (imm 2) (ind rax) ++
    movq !%r8 (ind ~ofs:(byte) rax) ++
    ret ++

    
    label func_list ++
    movq (ind rax) !%r10 ++
    cmpq (imm 5) !%r10 ++
    jne "runtime_error" ++
    movq (ind ~ofs:(byte) rax) !%rsi ++
    (*
    rsi: the value
    rcx: counter
    r8: current address
    rcx = rsi 
    rax: the address of the list
    *)
    movq !%rsi !%rdi ++
    addq (imm 2) !%rdi ++
    imulq (imm byte) !%rdi ++
    pushq !%rax ++
    pushq !%rsi ++
    call "malloc_wrapper" ++
    popq rsi ++
    popq rax ++
    movq (imm 4) (ind rax) ++
    movq !%rsi (ind ~ofs:(byte) rax) ++
    movq !%rax !%r8 ++
    addq (imm (2 * byte)) !%r8 ++
    xorq !%rcx !%rcx ++
    
    label func_list_loop ++
    cmpq !%rcx !%rsi ++
    je func_list_end ++
    movq (imm (2*byte)) !%rdi ++
    func_list_save_reg ++
    call "malloc_wrapper" ++
    movq !%rax !%r10 ++
    func_list_restore_reg ++
    movq (imm 2) (ind r10) ++
    movq !%rcx (ind ~ofs:(byte) r10) ++
    movq !%r10 (ind r8) ++
    incq !%rcx ++
    addq (imm byte) !%r8 ++
    jmp func_list_loop ++
    label func_list_end ++
    ret ++

    label func_list_get ++
    movq (ind rdi) !%r10 ++
    cmpq (imm 4) !%r10 ++
    jne "runtime_error" ++
    movq (ind rsi) !%r10 ++
    cmpq (imm 2) !%r10 ++
    jne "runtime_error" ++
    movq (ind ~ofs:(byte) rsi) !%r8 ++ (* index *)
    movq (ind ~ofs:(byte) rdi) !%r9 ++ (* len *)
    cmpl !%r8d !%r9d ++
    jle "runtime_error" ++
    addq (imm 2) !%r8 ++
    imulq (imm byte) !%r8 ++
    addq !%r8 !%rdi ++
    movq !%rdi !%rax ++
    ret ++

    difference env func_difference_eq (
      movq (imm 1) !%rax ++
      leave ++
      ret
    ) ++
    difference env func_difference_neq (
      movq (imm 1) !%rax ++
      leave ++
      ret  
    ) ++
    difference env func_difference_failure (jmp "runtime_error") ++
    
    label runtime_error ++
    leaq (lab "emit_runtime_error_msg") rdi ++
    xorq !%rax !%rax ++
    call "printf_wrapper" ++
    movq (imm 1) !%rdi ++
    call "exit"
  in

  let data = 
    label "none_string" ++
    string "None" ++
    label "true_string" ++
    string "True" ++
    label "false_string" ++
    string "False" ++
    label "format_int" ++
    string "%d" ++
    label "emit_runtime_error_msg" ++
    string "Runtime error\n"
  in 
  text, data

