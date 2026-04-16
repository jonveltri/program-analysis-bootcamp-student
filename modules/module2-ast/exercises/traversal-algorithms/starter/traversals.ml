(* traversals.ml - AST traversal algorithms exercise. *)

open Shared_ast.Ast_types

let string_of_op (op : op) : string =
  match op with
  | Add -> "+"  | Sub -> "-"  | Mul -> "*"  | Div -> "/"
  | Eq  -> "==" | Neq -> "!=" | Lt  -> "<"  | Gt  -> ">"
  | Le  -> "<=" | Ge  -> ">="
  | And -> "&&" | Or  -> "||"

let string_of_uop (uop : uop) : string =
  match uop with
  | Neg -> "-"
  | Not -> "!"

let label_of_expr (e : expr) : string =
  match e with
  | IntLit n -> Printf.sprintf "IntLit(%d)" n
  | BoolLit b -> Printf.sprintf "BoolLit(%b)" b
  | Var s -> Printf.sprintf "Var(%s)" s
  | BinOp (op, _, _) -> Printf.sprintf "BinOp(%s)" (string_of_op op)
  | UnaryOp (op, _) -> Printf.sprintf "UnaryOp(%s)" (string_of_uop op)
  | Call (name, _) -> Printf.sprintf "Call(%s)" name

let label_of_stmt (s : stmt) : string =
  match s with
  | Assign _ -> "Assign"
  | If _ -> "If"
  | While _ -> "While"
  | Return _ -> "Return"
  | Print _ -> "Print"
  | Block _ -> "Block"

(* --------------------------------------------------------------- *)
(* Pre-order DFS                                                    *)
(* --------------------------------------------------------------- *)

let rec pre_expr (e : expr) : string list =
  let here = label_of_expr e in
  match e with
  | IntLit _ | BoolLit _ | Var _ -> [here]
  | BinOp (_, e1, e2) -> here :: (pre_expr e1 @ pre_expr e2)
  | UnaryOp (_, e1) -> here :: pre_expr e1
  | Call (_, args) -> here :: List.concat_map pre_expr args

and pre_stmt (s : stmt) : string list =
  let here = label_of_stmt s in
  match s with
  | Assign (_, e) -> here :: pre_expr e
  | If (cond, then_b, else_b) ->
    here :: (pre_expr cond @ List.concat_map pre_stmt then_b
             @ List.concat_map pre_stmt else_b)
  | While (cond, body) ->
    here :: (pre_expr cond @ List.concat_map pre_stmt body)
  | Return None -> [here]
  | Return (Some e) -> here :: pre_expr e
  | Print exprs -> here :: List.concat_map pre_expr exprs
  | Block stmts -> here :: List.concat_map pre_stmt stmts

let pre_order (stmts : stmt list) : string list =
  List.concat_map pre_stmt stmts

(* --------------------------------------------------------------- *)
(* Post-order DFS                                                   *)
(* --------------------------------------------------------------- *)

let rec post_expr (e : expr) : string list =
  let here = label_of_expr e in
  match e with
  | IntLit _ | BoolLit _ | Var _ -> [here]
  | BinOp (_, e1, e2) -> post_expr e1 @ post_expr e2 @ [here]
  | UnaryOp (_, e1) -> post_expr e1 @ [here]
  | Call (_, args) -> List.concat_map post_expr args @ [here]

and post_stmt (s : stmt) : string list =
  let here = label_of_stmt s in
  match s with
  | Assign (_, e) -> post_expr e @ [here]
  | If (cond, then_b, else_b) ->
    post_expr cond @ List.concat_map post_stmt then_b
    @ List.concat_map post_stmt else_b @ [here]
  | While (cond, body) ->
    post_expr cond @ List.concat_map post_stmt body @ [here]
  | Return None -> [here]
  | Return (Some e) -> post_expr e @ [here]
  | Print exprs -> List.concat_map post_expr exprs @ [here]
  | Block stmts -> List.concat_map post_stmt stmts @ [here]

let post_order (stmts : stmt list) : string list =
  List.concat_map post_stmt stmts

(* --------------------------------------------------------------- *)
(* Breadth-first traversal                                          *)
(* --------------------------------------------------------------- *)

type node = E of expr | S of stmt

let children_of (n : node) : node list =
  match n with
  | E (IntLit _ | BoolLit _ | Var _) -> []
  | E (BinOp (_, e1, e2)) -> [E e1; E e2]
  | E (UnaryOp (_, e1)) -> [E e1]
  | E (Call (_, args)) -> List.map (fun e -> E e) args
  | S (Assign (_, e)) -> [E e]
  | S (If (cond, then_b, else_b)) ->
    E cond :: (List.map (fun s -> S s) then_b
               @ List.map (fun s -> S s) else_b)
  | S (While (cond, body)) ->
    E cond :: List.map (fun s -> S s) body
  | S (Return None) -> []
  | S (Return (Some e)) -> [E e]
  | S (Print exprs) -> List.map (fun e -> E e) exprs
  | S (Block stmts) -> List.map (fun s -> S s) stmts

let label_of_node (n : node) : string =
  match n with
  | E e -> label_of_expr e
  | S s -> label_of_stmt s

let bfs (stmts : stmt list) : string list =
  let q = Queue.create () in
  List.iter (fun s -> Queue.add (S s) q) stmts;
  let acc = ref [] in
  while not (Queue.is_empty q) do
    let n = Queue.pop q in
    acc := label_of_node n :: !acc;
    List.iter (fun c -> Queue.add c q) (children_of n)
  done;
  List.rev !acc
