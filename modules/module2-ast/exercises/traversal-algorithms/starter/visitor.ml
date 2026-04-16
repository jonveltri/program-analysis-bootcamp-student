(* visitor.ml - AST visitor pattern exercises. *)

open Shared_ast.Ast_types

let inc (key : string) (counts : (string * int) list) : (string * int) list =
  if List.mem_assoc key counts then
    List.map (fun (k, n) -> if k = key then (k, n + 1) else (k, n)) counts
  else
    counts @ [(key, 1)]

let rec count_expr (acc : (string * int) list) (e : expr) : (string * int) list =
  match e with
  | IntLit _ -> inc "IntLit" acc
  | BoolLit _ -> inc "BoolLit" acc
  | Var _ -> inc "Var" acc
  | BinOp (_, e1, e2) ->
    let acc = inc "BinOp" acc in
    let acc = count_expr acc e1 in
    count_expr acc e2
  | UnaryOp (_, e1) ->
    let acc = inc "UnaryOp" acc in
    count_expr acc e1
  | Call (_, args) ->
    let acc = inc "Call" acc in
    List.fold_left count_expr acc args

and count_stmt (acc : (string * int) list) (s : stmt) : (string * int) list =
  match s with
  | Assign (_, e) ->
    let acc = inc "Assign" acc in
    count_expr acc e
  | If (cond, then_b, else_b) ->
    let acc = inc "If" acc in
    let acc = count_expr acc cond in
    let acc = List.fold_left count_stmt acc then_b in
    List.fold_left count_stmt acc else_b
  | While (cond, body) ->
    let acc = inc "While" acc in
    let acc = count_expr acc cond in
    List.fold_left count_stmt acc body
  | Return None -> inc "Return" acc
  | Return (Some e) ->
    let acc = inc "Return" acc in
    count_expr acc e
  | Print exprs ->
    let acc = inc "Print" acc in
    List.fold_left count_expr acc exprs
  | Block stmts ->
    let acc = inc "Block" acc in
    List.fold_left count_stmt acc stmts

let count_nodes (stmts : stmt list) : (string * int) list =
  List.fold_left count_stmt [] stmts

let rec evaluate (e : expr) : int option =
  match e with
  | IntLit n -> Some n
  | UnaryOp (Neg, e1) ->
    (match evaluate e1 with
     | Some n -> Some (-n)
     | None -> None)
  | BinOp (op, e1, e2) ->
    (match evaluate e1, evaluate e2 with
     | Some a, Some b ->
       (match op with
        | Add -> Some (a + b)
        | Sub -> Some (a - b)
        | Mul -> Some (a * b)
        | Div -> if b = 0 then None else Some (a / b)
        | _ -> None)
     | _ -> None)
  | _ -> None
