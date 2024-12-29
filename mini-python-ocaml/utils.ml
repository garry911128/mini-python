open Format
open X86_64
open Ast
open Environment
open Const


let put_character (c:char): X86_64.text =
  movq (imm (Char.code c)) !%rdi ++
  call "putchar_wrapper"


let unique_label (env: env_t) (prefix: string) : string =
  let counter =
    match StringMap.find_opt prefix env.counters with
    | Some c -> c
    | None -> 0
  in
  env.counters <- StringMap.add prefix (counter + 1) env.counters;
  Printf.sprintf "%s%d" prefix counter


let arith_asm (code1:X86_64.text) (code2:X86_64.text) (instructions:X86_64.text) : X86_64.text =
  code1 ++
  movq (ind rax) !%r8 ++
  movq (ind ~ofs:(byte) rax) !%rdi ++
  pushq !%rdi ++
  pushq !%r8 ++ 
  code2 ++
  popq r8 ++
  popq rdi ++
  movq (ind rax) !%r9 ++
  movq (ind ~ofs:(byte) rax) !%rsi ++
  cmpq !%r8 !%r9 ++
  jne "runtime_error" ++
  cmpq (imm 2) !%r8 ++
  jne "runtime_error" ++
  instructions ++
  pushq (reg rdi) ++
  movq (imm byte) (reg rdi) ++
  call "malloc_wrapper" ++
  popq rdi ++
  movq (imm 2) (ind rax) ++
  movq (reg rdi) (ind ~ofs:(byte) rax)

let two_byte_operator_asm env (code1 :X86_64.text) (code2:X86_64.text) (instructions:X86_64.text) : X86_64.text =
  let two_byte = unique_label env "two_byte" in 
  let string_cmp = unique_label env "string_cmp" in 
  let end_label = unique_label env "end_label" in
  code1 ++
  movq (ind rax) !%r8 ++ 
  movq (ind ~ofs:(byte) rax) (reg rdi) ++
  pushq !%rdi ++
  pushq !%r8 ++
  code2 ++
  popq r8 ++
  popq rdi ++
  movq (ind rax) !%r9 ++
  movq (ind ~ofs:(byte) rax) !%rsi ++
  cmpq !%r8 !%r9 ++
  jne "runtime_error" ++
  cmpq (imm 0) !%r8 ++
  je "runtime_error" ++
  cmpq (imm 2) !%r8 ++
  jle two_byte ++
  cmpq (imm 3) !%r8 ++
  je string_cmp ++
  cmpq (imm 4) !%r8 ++
  jne "runtime_error" ++

  (* list cmp *)
  jmp end_label ++

  label two_byte ++ 
  instructions ++
  pushq (reg rdi) ++
  movq (imm byte) (reg rdi) ++
  call "malloc_wrapper" ++
  popq rdi ++
  movq (imm 1) (ind rax) ++
  movq (reg rdi) (ind ~ofs:(byte) rax) ++
  jmp end_label ++ 

  label string_cmp ++
  (* string cmp *)
  label end_label

let c_standard_function_wrapper (fn_name:string): X86_64.text =
  label (fn_name ^ "_wrapper") ++ 
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  andq (imm (-16)) (reg rsp) ++
  call fn_name ++
  movq (reg rbp) (reg rsp) ++
  popq rbp ++
  ret

let bool_builder i = 
  movq (imm (2*byte)) !%rdi ++
  call "malloc_wrapper" ++
  movq (imm 1) (ind rax) ++
  movq (imm i) (ind ~ofs:byte rax)

let difference env fn_label instructions =
  let func_diff_ty_eq = unique_label env "func_diff_ty_eq" in
  let func_diff_value_bool_int = unique_label env "func_eq_value_bool_int" in
  let func_diff_value_string = unique_label env "func_eq_value_string" in
  let func_diff_value_list_loop = unique_label env "func_eq_value_list_loop" in
  let func_diff_value_list_end = unique_label env "func_eq_value_list_end" in
  let func_diff_value_end = unique_label env "func_eq_value_end" in
  let counter_guard = unique_label env "func_counter" in
  label fn_label ++
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  movq (ind rdi) !%r10 ++
  movq (ind rsi) !%r11 ++
  cmpq !%r10 !%r11 ++
  je func_diff_ty_eq ++
  instructions ++
  label func_diff_ty_eq ++
  cmpq (imm 0) !%r10 ++
  je "runtime_error" ++
  cmpq (imm 2) !%r10 ++
  jle func_diff_value_bool_int ++
  cmpq (imm 3) !%r10 ++
  je func_diff_value_string ++
  cmpq (imm 4) !%r10 ++
  jne "runtime_error" ++
  movq (ind ~ofs:(byte) rdi) !%rcx ++
  movq (ind ~ofs:(byte) rsi) !%r10 ++
  cmpq !%r10 !%rcx ++
  jl counter_guard ++
  movq !%r10 !%rcx++
  label counter_guard ++
  movq !%rdi !%r8 ++
  addq (imm (2*byte)) !%r8 ++
  movq !%rsi !%r9 ++
  addq (imm (2*byte)) !%r9 ++

  label func_diff_value_list_loop ++
  testq !%rcx !%rcx ++
  jz func_diff_value_list_end ++
  pushq !%rdi ++
  pushq !%rsi ++
  movq (ind r8) !%rdi ++
  movq (ind r9) !%rsi ++
  pushq !%rcx ++
  pushq !%r8 ++
  pushq !%r9 ++
  call fn_label ++
  popq r9 ++
  popq r8 ++
  popq rcx ++
  popq rsi ++
  popq rdi ++
  cmpq (imm 0) !%rax ++
  jne func_diff_value_end ++
  decq !%rcx ++
  addq (imm (byte)) !%r8 ++
  addq (imm (byte)) !%r9 ++
  jmp func_diff_value_list_loop ++

  label func_diff_value_bool_int ++
  movq (ind ~ofs:byte rdi) !%rax ++
  movq (ind ~ofs:byte rsi) !%r9 ++
  subq !%r9 !%rax ++
  jmp func_diff_value_end ++

  label func_diff_value_string ++
  movq (ind ~ofs:(2 * byte) rdi) !%rdi ++
  movq (ind ~ofs:(2 * byte) rsi) !%rsi ++
  call "strcmp_wrapper" ++
  jmp func_diff_value_end ++

  label func_diff_value_list_end ++
  movq (ind ~ofs:byte rdi) !%rax ++
  movq (ind ~ofs:byte rsi) !%r9 ++
  subq !%r9 !%rax ++

  label func_diff_value_end ++
  leave ++
  ret