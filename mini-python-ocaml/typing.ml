open Ast

let debug = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string

(* 用於報告類型錯誤 *)
let error ?(loc=dummy_loc) f =
  Format.kasprintf (fun s -> raise (Error (loc, s))) ("@[" ^^ f ^^ "@]")

(* 定義環境結構 *)
type env = {
  vars: (string, var) Hashtbl.t;  (* 變數環境 *)
  fns: (string, fn) Hashtbl.t;    (* 函數環境 *)
}

(* 初始化環境 *)
let init_env () = 
  print_endline "Initializing environment...";
  { vars = Hashtbl.create 16; fns = Hashtbl.create 16 }

(* 查找變量 *)
let find_var env id =
  print_endline ("Looking for variable: " ^ id);
  try Some (Hashtbl.find env.vars id)
  with Not_found -> None

(* 添加變量 *)
let add_var env id var =
  print_endline ("Adding variable: " ^ id);
  Hashtbl.add env.vars id var

(* 查找函數 *)
let find_fn env id =
  print_endline ("Looking for function: " ^ id);
  try Some (Hashtbl.find env.fns id)
  with Not_found -> None

(* 添加函數 *)
let add_fn env id fn =
  print_endline ("Adding function: " ^ id);
  Hashtbl.add env.fns id fn

(* 類型檢查表達式 *)
let rec type_check_expr env (e: expr) : texpr =
  print_endline "Type checking expression";
  match e with
  | Ecst c -> 
      print_endline "Expression is a constant";
      TEcst c
  | Eident id ->
      print_endline ("Expression is an identifier: " ^ id.id);
      begin match find_var env id.id with
      | Some v -> TEvar v
      | None -> error ~loc:id.loc "unbound variable %s" id.id
      end 
  | Ebinop (op, e1, e2) ->
      print_endline "Expression is a binary operation";
      let te1 = type_check_expr env e1 in
      let te2 = type_check_expr env e2 in
      TEbinop (op, te1, te2)
  | Eunop (op, e) ->
      print_endline "Expression is a unary operation";
      let te = type_check_expr env e in
      TEunop (op, te)
  | Ecall (id, args) ->
    print_endline ("Expression is a function call: " ^ id.id);
    begin match find_fn env id.id with
    | Some fn ->
        (* 检查参数数量是否一致 *)
        if List.length args <> List.length fn.fn_params then
          error ~loc:id.loc "function %s expects %d arguments but got %d"
            id.id (List.length fn.fn_params) (List.length args);
        
        let targs = List.map (type_check_expr env) args in
        TEcall (fn, targs)
    | None -> error ~loc:id.loc "unbound function %s" id.id
    end    
  | Elist lst ->
      print_endline "Expression is a list";
      let tlst = List.map (type_check_expr env) lst in
      TElist tlst
  | Eget (e1, e2) ->
      print_endline "Expression is a get operation";
      let te1 = type_check_expr env e1 in
      let te2 = type_check_expr env e2 in
      TEget (te1, te2)

(* 類型檢查語句 *)
let rec type_check_stmt env (s: stmt) : tstmt =
  print_endline "Type checking statement";
  match s with
  | Sif (cond, then_stmt, else_stmt) ->
      print_endline "Statement is an if statement";
      let tcond = type_check_expr env cond in
      let tthen = type_check_stmt env then_stmt in
      let telse = type_check_stmt env else_stmt in
      TSif (tcond, tthen, telse)
  | Sreturn expr ->
      print_endline "Statement is a return statement";
      let texpr = type_check_expr env expr in
      TSreturn texpr
  | Sassign (id, expr) ->
      print_endline ("Statement is an assignment to " ^ id.id);
      let texpr = type_check_expr env expr in
      begin match find_var env id.id with
      | Some var -> TSassign (var, texpr)
      | None ->
          (* 如果變量尚未定義，則創建新的變量 *)
          let var = { v_name = id.id; v_ofs = 0 } in
          add_var env id.id var;
          TSassign (var, texpr)
      end
  | Sprint expr ->
      print_endline "Statement is a print statement";
      let texpr = type_check_expr env expr in
      TSprint texpr
  | Sblock stmts ->
      print_endline "Statement is a block";
      let tstmts = List.map (type_check_stmt env) stmts in
      TSblock tstmts
  | Sfor (id, expr, body) ->
      print_endline "Statement is a for loop";
      let texpr = type_check_expr env expr in
      (* 創建新的變量並添加到環境中 *)
      let var = { v_name = id.id; v_ofs = 0 } in
      add_var env id.id var;
      let tbody = type_check_stmt env body in
      TSfor (var, texpr, tbody)
  | Seval expr ->
      print_endline "Statement is an eval statement";
      let texpr = type_check_expr env expr in
      TSeval texpr
  | Sset (e1, e2, e3) ->
      print_endline "Statement is a set operation";
      let te1 = type_check_expr env e1 in
      let te2 = type_check_expr env e2 in
      let te3 = type_check_expr env e3 in
      TSset (te1, te2, te3)

(* 類型檢查函數定義 *)
let type_check_def env (id, params, body) : tdef =
  print_endline ("Type checking function definition: " ^ id.id);
  (* 建立函數記錄 *)
  let fn_params = List.map (fun param ->
    let var = { v_name = param.id; v_ofs = 0 } in
    add_var env param.id var;
    var
  ) params in
  let fn = { fn_name = id.id; fn_params } in
  add_fn env id.id fn;
  let tbody = type_check_stmt env body in
  (fn, tbody)

(* 處理整個文件 *)
let file ?debug:(b=false) (p: Ast.file) : Ast.tfile =
  debug := b;
  print_endline "Starting type checking for the file...";
  let env = init_env () in
  let defs, stmts = p in
  let tdefs = List.map (type_check_def env) defs in
  let main_body = type_check_stmt env (Sblock [stmts]) in
  print_endline "Finished type checking the file.";
  tdefs @ [( { fn_name = "main"; fn_params = [] }, main_body )]