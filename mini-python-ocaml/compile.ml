
open Format
open X86_64
open Ast

let debug = ref false

module StringMap = Map.Make(String)

type env_t = {
  mutable vars: (var * int * int) StringMap.t;
  funcs: fn StringMap.t;
  mutable stack_offset: int;
  mutable string_counter: int;
}

let empty_env = {
  vars = StringMap.empty;
  funcs = StringMap.empty;
  stack_offset = 0;
  string_counter = 0;
}
let byte = 8
let unique_string_label (env:env_t) (s:string) : X86_64.data =
  let n = env.string_counter in
  env.string_counter <- n + 1;
  label (Printf.sprintf "str%d" n)

(* const is always stored at the address rax is pointing to *)
let compile_const (env: env_t) (c: Ast.constant) : X86_64.text * X86_64.data * int =
  match c with
  | Cnone ->
    env.stack_offset <- env.stack_offset + byte;
    movq (imm byte) (reg rdi) ++
    call "malloc_wrapper" ++
    movq (imm 0) (ind rax) ++
    movq (reg rax) (ind ~ofs:(-env.stack_offset) rbp)
    , nop, 0
  | Cbool b ->
    env.stack_offset <- env.stack_offset + byte;
    movq (imm byte) (reg rdi) ++
    call "malloc_wrapper" ++
    movq (imm (if b then 1 else 0)) (ind rax) ++
    movq (reg rax) (ind ~ofs:(-env.stack_offset) rbp)
    , nop, 1
  | Cint i ->
    env.stack_offset <- env.stack_offset + byte;
    movq (imm byte) (reg rdi) ++
    call "malloc_wrapper" ++
    movq (imm64 i) (ind ~ofs:(0) rax) ++
    movq (reg rax) (ind ~ofs:(-env.stack_offset) rbp)
    , nop, 2
  | Cstring s ->
    leaq (lab (Printf.sprintf "str%d" (env.string_counter - 1))) rax,
    unique_string_label env s ++ string s, 3

let compile_var (env: env_t) (v: Ast.var) : X86_64.text * X86_64.data * int =
  if StringMap.mem v.v_name env.vars then
    begin
      let var, ofs, var_type = StringMap.find v.v_name env.vars in
      (* for now, only return the type *)
      if var_type = 2 then
        nop, nop, var_type
      else
        failwith "Unsupported var type"
    end
  else
    failwith "Variable not found"

let rec compile_expr (env: env_t) (expr: Ast.texpr) : X86_64.text * X86_64.data * int =
  match expr with
  | TEcst c ->
    compile_const env c
  | TEvar v ->
    compile_var env v
  | TEbinop (op, e1, e2) ->
    begin match op with
    | Badd ->
      let text_code1, data_code1, expr_type1 = compile_expr env e1 in
      let text_code2, data_code2, expr_type2 = compile_expr env e2 in
      if expr_type1 = 2 && expr_type2 = 2 then
        text_code1 ++
        movq (ind rax) (reg rdi) ++
        pushq (reg rdi) ++
        text_code2 ++
        popq rdi ++
        movq (ind rax) (reg rsi) ++
        addq (reg rsi) (reg rdi) ++
        pushq (reg rdi) ++
        movq (imm byte) (reg rdi) ++
        call "malloc_wrapper" ++
        popq rdi ++
        movq (reg rdi) (ind rax),
        data_code1 ++ data_code2, 2
      else
        failwith "Unsupported Badd"
    | Bsub ->
      let text_code1, data_code1, expr_type1 = compile_expr env e1 in
      let text_code2, data_code2, expr_type2 = compile_expr env e2 in
      if expr_type1 = 2 && expr_type2 = 2 then
        text_code1 ++
        movq (ind rax) (reg rdi) ++
        pushq (reg rdi) ++
        text_code2 ++
        popq rdi ++
        movq (ind rax) (reg rsi) ++
        subq (reg rsi) (reg rdi) ++
        pushq (reg rdi) ++
        movq (imm byte) (reg rdi) ++
        call "malloc_wrapper" ++
        popq rdi ++
        movq (reg rdi) (ind rax), 
        data_code1 ++ data_code2, 2
      else
        failwith "Unsupported Bsub"
    | Bmul ->
      let text_code1, data_code1, expr_type1 = compile_expr env e1 in
      let text_code2, data_code2, expr_type2 = compile_expr env e2 in
      if expr_type1 = 2 && expr_type2 = 2 then
        text_code1 ++
        movq (ind rax) (reg rdi) ++
        pushq (reg rdi) ++
        text_code2 ++
        popq rdi ++
        movq (ind rax) (reg rsi) ++
        imulq (reg rsi) (reg rdi) ++
        pushq (reg rdi) ++
        movq (imm byte) (reg rdi) ++
        call "malloc_wrapper" ++
        popq rdi ++
        movq (reg rdi) (ind rax), 
        data_code1 ++ data_code2, 2
      else
        failwith "Unsupported Bmul"
    | Bdiv ->
      let text_code1, data_code1, expr_type1 = compile_expr env e1 in
      let text_code2, data_code2, expr_type2 = compile_expr env e2 in
      if expr_type1 = 2 && expr_type2 = 2 then
        text_code1 ++
        movq (ind rax) (reg rdi) ++
        pushq (reg rdi) ++
        text_code2 ++
        popq rdi ++
        movq (ind rax) (reg rsi) ++
        movq (reg rdi) (reg rax) ++
        cqto ++
        movq (reg rsi) (reg rbx) ++
        idivq (reg rbx) ++
        movq (reg rax) (reg rdi) ++
        pushq (reg rdi) ++
        movq (imm byte) (reg rdi) ++
        call "malloc_wrapper" ++
        popq rdi ++
        movq (reg rdi) (ind rax), 
        data_code1 ++ data_code2, 2
      else
        failwith "Unsupported Bdiv"
    | Bmod ->
      let text_code1, data_code1, expr_type1 = compile_expr env e1 in
      let text_code2, data_code2, expr_type2 = compile_expr env e2 in
      if expr_type1 = 2 && expr_type2 = 2 then
        text_code1 ++
        movq (ind rax) (reg rdi) ++
        pushq (reg rdi) ++
        text_code2 ++
        popq rdi ++
        movq (ind rax) (reg rsi) ++
        movq (reg rdi) (reg rax) ++
        cqto ++
        movq (reg rsi) (reg rbx) ++
        idivq (reg rbx) ++
        movq (reg rdx) (reg rdi) ++
        pushq (reg rdi) ++
        movq (imm byte) (reg rdi) ++
        call "malloc_wrapper" ++
        popq rdi ++
        movq (reg rdi) (ind rax), 
        data_code1 ++ data_code2, 2
      else
        failwith "Unsupported Bmod"
    | Beq ->
      let text_code1, data_code1, expr_type1 = compile_expr env e1 in
      let text_code2, data_code2, expr_type2 = compile_expr env e2 in
      if expr_type1 = 2 && expr_type2 = 2 then
        text_code1 ++
        movq (ind rax) (reg rdi) ++
        pushq (reg rdi) ++
        text_code2 ++
        popq rdi ++
        movq (ind rax) (reg rsi) ++
        cmpq (reg rsi) (reg rdi) ++
        sete (reg dil) ++
        movzbq (reg dil) rdi ++
        pushq (reg rdi) ++
        movq (imm byte) (reg rdi) ++
        call "malloc_wrapper" ++
        popq rdi ++
        movq (reg rdi) (ind rax), 
        data_code1 ++ data_code2, 2
      else
        failwith "Unsupported Beq"
    | Bneq ->
      failwith "Unsupported Bneq"
    | Blt ->
      failwith "Unsupported Blt"
    | Ble ->
      failwith "Unsupported Ble"
    | Bgt ->
      failwith "Unsupported Bgt"
    | Bge ->
      failwith "Unsupported Bge"
    | Band ->
      failwith "Unsupported Band"
    | Bor ->
      failwith "Unsupported Bor"
    end
  | TEunop (op, e) ->
    failwith "Unsupported TEunop"
  | TEcall (fn, args) ->
    failwith "Unsupported TEcall"
  | TElist l ->
    failwith "Unsupported TElist"
  | TErange e ->
    failwith "Unsupported TErange"
  | TEget (e1, e2) ->
    failwith "Unsupported TEget"

let rec compile_stmt (env: env_t) (stmt: Ast.tstmt) : X86_64.text * X86_64.data =
  match stmt with
  | TSif (cond, s1, s2) ->
    failwith "Unsupported TSif"
  | TSreturn expr ->
    failwith "Unsupported Sreturn"
  | TSassign (var, expr) -> (* x = 1 *)
    let text_code, data_code, expr_type = compile_expr env expr in
    let ofs = -env.stack_offset in
    env.vars <- StringMap.add var.v_name (var, ofs, expr_type) env.vars;
    text_code, data_code
  | TSprint expr ->
    let text_code, data_code, expr_type = compile_expr env expr in
    (* format string required *)
    comment "print" ++
    text_code ++
    movq (ind rax) (reg rsi) ++
    leaq (lab "print_int") rdi ++
    call "printf_wrapper",
    data_code
  | TSblock stmts ->
    List.fold_left (fun (acc_text, acc_data) stmt ->
      let text_code, data_code = compile_stmt env stmt in
      acc_text ++ text_code, acc_data ++ data_code
    ) (nop, nop) stmts
    |> fun (text_code, data_code) -> 
      subq (imm (env.stack_offset)) (reg rsp) ++
      text_code ++
      addq (imm (env.stack_offset)) (reg rsp)
      , data_code
  | TSfor (var, expr, body) ->
    failwith "Unsupported Sfor"
  | TSeval expr ->
    failwith "Unsupported TSeval"
  | TSset (e1, e2, e3) ->
    failwith "Unsupported TSset"

let compile_def env ((fn, body): Ast.tdef) : X86_64.text * X86_64.data =
  let env_local = { env with vars = StringMap.empty; } in
  let prologue = 
    pushq (reg rbp) ++
    movq (reg rsp) (reg rbp) in
  let epilogue = 
    xorq (reg rax) (reg rax) ++
    movq (reg rbp) (reg rsp) ++
    popq rbp ++
    ret in
  let body_code, data_code = compile_stmt env_local body in
  label fn.fn_name ++ prologue ++ body_code ++ epilogue, data_code

let malloc_wrapper : X86_64.text =
  label "malloc_wrapper" ++
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  andq (imm (-16)) (reg rsp) ++
  comment "allign rsp to 16 bytes" ++
  call "malloc" ++
  testq (reg rax) (reg rax) ++
  movq (reg rbp) (reg rsp) ++
  popq rbp ++
  ret

let printf_wrapper: X86_64.text =
  label "printf_wrapper" ++
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  andq (imm (-16)) (reg rsp) ++
  comment "allign rsp to 16 bytes" ++
  call "printf" ++
  testq (reg rax) (reg rax) ++
  movq (reg rbp) (reg rsp) ++
  popq rbp ++
  ret

let file ?debug:(b=false) (p: Ast.tfile) : X86_64.program =
  debug := b;
  let env = empty_env in
  (* Compile each function *)
  let text_code, data_code = List.fold_left (fun (text_acc, data_acc) (fn, body) ->
    let fn_code, data_code = compile_def env (fn, body) in
    (text_acc ++ fn_code, data_acc ++ data_code)
  ) (nop, nop) p in
  { 
    text = 
      malloc_wrapper ++
      printf_wrapper ++
      globl "main" ++ text_code;
    data = 
      data_code ++
      label "print_int" ++
      string "%d\n"
      (* hard-coded print_int *)
  }
