
open Format
open X86_64
open Ast
open Utils
open Environment
open Functions

let debug = ref false

let rec compile_expr (env: env_t) (parent_env:env_t) (expr: Ast.texpr) : X86_64.text * X86_64.data =
  match expr with
  | TEcst c ->
    begin match c with
    | Cnone ->
      movq (imm (2 * byte))  !%rdi ++
      call "malloc_wrapper" ++
      movq (imm 0) (ind rax) ++
      movq (imm 0) (ind ~ofs:(byte) rax)
      , nop
    | Cbool b ->
      movq (imm (2 * byte)) !%rdi ++
      call "malloc_wrapper" ++
      movq (imm 1) (ind rax) ++
      movq (imm (if b then 1 else 0)) (ind ~ofs:(byte) rax)
      , nop
    | Cint i ->
      movq (imm (2 * byte)) !%rdi ++
      call "malloc_wrapper" ++
      movq (imm 2) (ind rax) ++
      movq (imm64 i) (ind ~ofs:(byte) rax)
      , nop
    | Cstring s ->
      let len = String.length s in
      let string_label = unique_label env "string" in
      movq (imm (3 * byte)) !%rdi ++
      call "malloc_wrapper" ++
      movq (imm 3) (ind rax) ++
      movq (imm len) (ind ~ofs:(byte) rax) ++
      leaq (lab string_label) rdi ++
      movq !%rdi (ind ~ofs:(2 * byte) rax)
      , label string_label ++ string s
    end
  | TEvar v ->
    let var, ofs = StringMap.find v.v_name env.vars in
    movq (ind ~ofs:(ofs+parent_env.stack_offset) rbp) !%rax, nop
  | TEbinop (op, e1, e2) ->
    begin match op with
    | Band ->
      let text_code1, data_code1 = compile_expr env parent_env e1 in
      let text_code2, data_code2 = compile_expr env parent_env e2 in
      let l = unique_label env "cond_false" in
      text_code1 ++
      movq (ind ~ofs:(byte) rax) !%rdi ++
      cmpq (imm 0) !%rdi ++
      je l ++
      text_code2 ++
      label l, 
      data_code1 ++ data_code2
    | Bor ->
      let text_code1, data_code1 = compile_expr env parent_env e1 in
      let text_code2, data_code2 = compile_expr env parent_env e2 in
      let l = unique_label env "cond_true" in
      text_code1 ++
      movq (ind ~ofs:(byte) rax) !%rdi ++
      cmpq (imm 0) !%rdi ++
      jne l ++
      text_code2 ++
      label l,
      data_code1 ++ data_code2
    | Badd | Bsub | Bmul | Bdiv | Bmod | Beq | Bneq | Blt | Ble | Bgt | Bge ->
      let text_code1, data_code1 = compile_expr env parent_env e1 in
      let text_code2, data_code2 = compile_expr env parent_env e2 in
      begin match op with
      | Badd ->
        let end_label = unique_label env "end_label" in
        let int_label = unique_label env "int" in
        let string_label = unique_label env "string" in
        text_code1 ++
        movq !%rax !%rdi ++
        pushq !%rdi ++
        text_code2 ++
        popq rdi ++
        movq !%rax !%rsi ++
        movq (ind rdi) !%r10 ++
        movq (ind rsi) !%r11 ++
        cmpq !%r10 !%r11 ++
        jne "runtime_error" ++
        cmpq (imm 2) !%r10 ++
        je int_label ++
        cmpq (imm 3) !%r10 ++
        je string_label ++
        cmpq (imm 4) !%r10 ++
        jne "runtime_error" ++
        call "concat_list" ++
        jmp end_label ++

        label int_label ++
        movq (ind ~ofs:(byte) rdi) !%rdi ++
        addq (ind ~ofs:(byte) rsi) !%rdi ++
        pushq (reg rdi) ++
        movq (imm (2*byte)) (reg rdi) ++
        call "malloc_wrapper" ++
        popq rdi ++
        movq (imm 2) (ind rax) ++
        movq (reg rdi) (ind ~ofs:(byte) rax) ++
        jmp end_label ++
        

        label string_label ++
        movq (ind ~ofs:(byte) rdi) !%r10 ++
        addq (ind ~ofs:(byte) rsi) !%r10 ++
        movq !%r10 !%rcx ++
        addq (imm 1) !%r10 ++
        pushq !%rdi ++
        pushq !%rsi ++
        pushq !%r10 ++
        movq !%r10 !%rdi ++
        call "malloc_wrapper" ++
        popq r10 ++
        popq rsi ++
        popq rdi ++
        movq (ind ~ofs:(2*byte) rdi) !%rdi ++
        movq (ind ~ofs:(2*byte) rsi) !%rsi ++
        pushq !%rdi ++
        pushq !%rsi ++
        pushq !%rcx ++
        movq !%rdi !%rsi ++
        movq !%rax !%rdi ++
        call "strcpy_wrapper" ++
        popq rcx ++
        popq rsi ++
        popq rdi ++
        movq !%rax !%rdi ++
        call "strcat_wrapper" ++
        movq (imm (3*byte)) !%rdi ++
        movq !%rax !%rsi ++
        pushq !%rsi ++
        pushq !%rcx ++
        call "malloc_wrapper" ++
        popq rcx ++
        popq rsi ++
        movq (imm 3) (ind rax) ++
        movq !%rcx (ind ~ofs:byte rax) ++
        movq !%rsi (ind ~ofs:(2 * byte) rax) ++
        label end_label
        , data_code1 ++ data_code2
      | Bsub ->
        arith_asm text_code1 text_code2 (subq !%rsi !%rdi),
        data_code1 ++ data_code2
      | Bmul ->
        arith_asm text_code1 text_code2 (imulq !%rsi !%rdi),
        data_code1 ++ data_code2
      | Bdiv ->
        arith_asm text_code1 text_code2
        (
          movq !%rdi !%rax ++
          cqto ++
          movq !%rsi !%rbx ++
          idivq !%rbx ++
          movq !%rax !%rdi
        ),
        data_code1 ++ data_code2
      | Bmod ->
        arith_asm text_code1 text_code2
        (
          movq !%rdi !%rax ++
          cqto ++
          movq !%rsi !%rbx ++
          idivq !%rbx ++
          movq !%rdx !%rdi
        ),
        data_code1 ++ data_code2
      | Beq ->
        two_byte_operator_asm env text_code1 text_code2
        (
          cmpq !%rsi !%rdi ++
          sete !%dil ++
          movzbq !%dil rdi
        ),
        data_code1 ++ data_code2
      | Bneq ->
        two_byte_operator_asm env text_code1 text_code2
        (
          cmpq !%rsi !%rdi ++
          setne !%dil ++
          movzbq !%dil rdi
        ),
        data_code1 ++ data_code2
      | Blt ->
        two_byte_operator_asm env text_code1 text_code2
        (
          cmpq !%rsi !%rdi ++
          setl !%dil ++
          movzbq !%dil rdi
        ),
        data_code1 ++ data_code2
      | Ble ->
        two_byte_operator_asm env text_code1 text_code2
        (
          cmpq !%rsi !%rdi ++
          setle !%dil ++
          movzbq !%dil rdi
        ),
        data_code1 ++ data_code2
      | Bgt ->
        two_byte_operator_asm env text_code1 text_code2
        (
          cmpq !%rsi !%rdi ++
          setg !%dil ++
          movzbq !%dil rdi
        ),
        data_code1 ++ data_code2
      | Bge ->
        two_byte_operator_asm env text_code1 text_code2
        (
          cmpq !%rsi !%rdi ++
          setge !%dil ++
          movzbq !%dil rdi
        ),
        data_code1 ++ data_code2
      | _ -> failwith "Unsupported binop"
      end
      end
  | TEunop (op, e) ->
    begin match op with
    | Uneg ->
      let text_code, data_code = compile_expr env parent_env e in
        text_code ++
        movq (ind rax) !%r10 ++
        cmpq (imm 2) !%r10 ++
        jne "runtime_error" ++
        movq (ind ~ofs:(byte) rax) !%rdi ++
        negq !%rdi ++
        pushq !%rdi ++
        movq (imm byte) !%rdi ++
        call "malloc_wrapper" ++
        popq rdi ++
        movq (imm 2) (ind rax) ++
        movq !%rdi (ind ~ofs:(byte) rax),
        data_code
    | Unot ->
      let text_code, data_code = compile_expr env parent_env e in
      text_code ++
      movq (ind rax) !%r10 ++
      cmpq (imm 1) !%r10 ++
      jne "runtime_error" ++
      movq (ind ~ofs:(byte) rax) !%rdi ++
      xorq (imm 1) !%rdi ++
      pushq !%rdi ++
      movq (imm byte) !%rdi ++
      call "malloc_wrapper" ++
      popq rdi ++
      movq (imm 1) (ind rax) ++
      movq !%rdi (ind ~ofs:(byte) rax),
      data_code
    end
  | TEcall (fn, args) ->
    begin match fn.fn_name with
    | "len" ->
      if List.length args = 1 then
        let text_code, data_code = compile_expr env parent_env (List.hd args) in
        text_code ++
        func_len
        , data_code
      else failwith "len function takes exactly one argument"
    | "list" ->
      if List.length args = 1 then
        let text_code, data_code = compile_expr env parent_env (List.hd args) in
        text_code ++
        func_list env
        , data_code
      else failwith "list function takes exactly one argument"
    | _ ->
      failwith "Unsupported function call"
    end
  | TElist l ->
    let len = List.length l in
    List.fold_left (fun (text_acc, data_acc, counter) i ->
      let text_expr, data_expr = compile_expr env parent_env i in
      text_acc ++
      pushq !%rax ++ (* save heap address*)
      text_expr ++
      movq !%rax !%rsi ++
      popq rax ++
      movq !%rsi (ind ~ofs:(counter) rax),
      data_acc ++ data_expr, counter + byte
    ) (nop, nop, 2 * byte) l |> fun (text_code, data_code, _) ->
    movq (imm ((len + 2) * byte)) !%rdi ++
    call "malloc_wrapper" ++
    movq (imm 4) (ind rax) ++ (* type *)
    movq (imm len) (ind ~ofs:(byte) rax) ++ (* length *)
    text_code, data_code
  | TErange e ->
    let text_code, data_code = compile_expr env parent_env e in
    text_code ++
    movq (ind rax) !%r10++
    cmpq (imm 2) !%r10 ++
    jne "runtime_error" ++
    movq (ind ~ofs:(byte) rax) !%r10 ++
    movq (imm (2 * byte)) !%rdi ++
    call "malloc_wrapper" ++
    movq (imm 5) (ind rax) ++
    movq !%r10 (ind ~ofs:(byte) rax), data_code
  | TEget (e1, e2) ->
    failwith "Unsupported TEget"

let rec compile_stmt (env: env_t) (parent_env:env_t) (stmt: Ast.tstmt) : X86_64.text * X86_64.data =
  match stmt with
  | TSif (cond, s1, s2) ->
    let text_code_cond, data_code_cond = compile_expr env parent_env cond in
    let text_code_s1, data_code_s1 = compile_stmt env parent_env s1 in
    let text_code_s2, data_code_s2 = compile_stmt env parent_env s2 in
    let else_label = unique_label env "else" in
    let end_label = unique_label env "end" in
    text_code_cond ++
    cmpq (imm 0) (ind rax) ++
    je else_label ++
    text_code_s1 ++
    jmp end_label ++
    label else_label ++
    text_code_s2 ++
    label end_label
    , data_code_cond ++ data_code_s1 ++ data_code_s2
  | TSreturn expr ->
    failwith "Unsupported Sreturn"
  | TSassign (var, expr) ->
    env.stack_offset <- env.stack_offset - byte;
    let text_code, data_code = compile_expr env parent_env expr in
    env.vars <- StringMap.add var.v_name (var, env.stack_offset) env.vars;
    text_code ++
    movq !%rax (ind ~ofs:(env.stack_offset) rbp)
    , data_code
  | TSprint expr ->
    let text_code, data_code = compile_expr env parent_env expr in
    text_code ++
    movq !%rax !%rdi ++
    call "print_value" ++
    put_character '\n',
    data_code
  | TSblock stmts ->
    List.fold_left (fun (acc_text, acc_data) stmt ->
      let text_code, data_code = compile_stmt env parent_env stmt in
      acc_text ++ text_code, acc_data ++ data_code
    ) (nop, nop) stmts
    |> fun (text_code, data_code) -> 
      text_code
      , data_code
  | TSfor (var, expr, body) ->
    let expr_text_code, expr_data_code = compile_expr env parent_env expr in
    (* let env_local = {
      vars = StringMap.add var.v_name (var, -byte) env.vars;
      funcs = StringMap.empty;
      stack_offset = -byte;
      counters = StringMap.empty;
    } in *)
    begin try
      let found_var, found_ofs = StringMap.find var.v_name env.vars in 
      let body_text_code, body_data_code = compile_stmt env parent_env body in
      let loop_label = unique_label env "loop" in
      let end_label = unique_label env "end" in
      let do_jump = unique_label env "do_jump" in 
      comment "for loop" ++
      expr_text_code ++
      movq !%rax !%rdi ++
      movq (ind ~ofs:byte rax) !%rcx ++
      addq (imm (2 * byte)) !%rdi ++
      movq !%rdi !%rsi ++
      movq (ind rdi) !%r10 ++
      movq !%r10 (ind ~ofs:(found_ofs) rbp) ++
      jmp do_jump ++

      label loop_label ++
      testq !%rcx !%rcx ++
	    jz end_label ++
      addq (imm byte) !%rsi ++
      movq (ind rsi) !%r10 ++
      movq !%r10 (ind ~ofs:(found_ofs) rbp) ++
      
      label do_jump ++
      testq !%rcx !%rcx ++
      jz end_label ++
      pushq !%rcx ++
      pushq !%rdi ++
      pushq !%rsi ++
      comment "body" ++
      body_text_code ++
      comment "end body" ++
      popq rsi ++
      popq rdi ++
      popq rcx ++
      decq !%rcx ++
      
      jmp loop_label ++
      label end_label
      ,expr_data_code ++ body_data_code
    with Not_found ->
      env.stack_offset <- env.stack_offset - byte;
      env.vars <- StringMap.add var.v_name (var, env.stack_offset) env.vars;
      let body_text_code, body_data_code = compile_stmt env parent_env body in
      let loop_label = unique_label env "loop" in
      let end_label = unique_label env "end" in
      let do_jump = unique_label env "do_jump" in 
      comment "for loop" ++
      expr_text_code ++
      movq !%rax !%rdi ++
      movq (ind ~ofs:byte rax) !%rcx ++
      addq (imm (2 * byte)) !%rdi ++
      movq !%rdi !%rsi ++
      movq (ind rdi) !%r10 ++
      movq !%r10 (ind ~ofs:(env.stack_offset) rbp) ++
      jmp do_jump ++

      label loop_label ++
      testq !%rcx !%rcx ++
	    jz end_label ++
      addq (imm byte) !%rsi ++
      movq (ind rsi) !%r10 ++
      movq !%r10 (ind ~ofs:(env.stack_offset) rbp) ++
      
      label do_jump ++
      testq !%rcx !%rcx ++
      jz end_label ++
      pushq !%rcx ++
      pushq !%rdi ++
      pushq !%rsi ++
      comment "body" ++
      body_text_code ++
      comment "end body" ++
      popq rsi ++
      popq rdi ++
      popq rcx ++
      decq !%rcx ++
      
      jmp loop_label ++
      label end_label
      ,expr_data_code ++ body_data_code
    end;
  | TSeval expr ->
    failwith "Unsupported TSeval"
  | TSset (e1, e2, e3) ->
    failwith "Unsupported TSset"

let compile_def env ((fn, body): Ast.tdef) : X86_64.text * X86_64.data =
  let env_global = {
    vars = StringMap.empty;
    funcs = StringMap.empty;
    stack_offset = 0;
    counters = StringMap.empty;
  } in
  let body_code, data_code = compile_stmt env env_global body in
  let prologue = 
    pushq !%rbp ++
    movq !%rsp !%rbp ++
    addq (imm (env.stack_offset)) !%rsp
  in
  let epilogue = 
    subq (imm (env.stack_offset)) !%rsp ++
    xorq !%rax !%rax ++
    movq !%rbp !%rsp ++
    popq rbp ++
    ret in
  label fn.fn_name ++ prologue ++ body_code ++ epilogue, data_code


let file ?debug:(b=false) (p: Ast.tfile) : X86_64.program =
  debug := b;
  let env = empty_env in
  let runtime_error_text, runtime_error_data = emit_runtime_error env in
  (* Compile each function *)
  let text_code, data_code = List.fold_left (fun (text_acc, data_acc) (fn, body) ->
    let fn_code, data_code = compile_def env (fn, body) in
    (text_acc ++ fn_code, data_acc ++ data_code)
  ) (nop, nop) p in
  { 
    text = 
      c_standard_function_wrapper "malloc" ++
      c_standard_function_wrapper "putchar" ++
      c_standard_function_wrapper "printf" ++
      c_standard_function_wrapper "strcmp" ++
      c_standard_function_wrapper "strcpy" ++
      c_standard_function_wrapper "strcat" ++
      runtime_error_text ++
      func_copy_two_byte ++
      func_copy_string ++
      func_copy_value env ++
      func_list_concat env ++
      func_print_value env ++
      globl "main" ++ text_code;    
    data = 
      data_code ++
      func_print_none_data ++
      func_print_bool_data ++
      func_print_int_data ++
      runtime_error_data ++
      inline "\t.section .note.GNU-stack,\"\",@progbits\n";
  }