(* Copyright (c) 2025 Groupoid Infinity *)

type typ =
  | TInt | TBool | TString | TSet of typ | TFun of typ * typ | TRec of (string * typ) list
  | TVar of tvar ref | TFormula
and tvar = Free of int | Link of typ

let new_tvar =
  let counter = ref 0 in
  fun () -> incr counter; TVar (ref (Free !counter))

exception TypeError of string

let rec occur_check v = function
  | TVar r -> (match !r with Free id -> id = v | Link t -> occur_check v t)
  | TSet t -> occur_check v t
  | TFun (t1, t2) -> occur_check v t1 || occur_check v t2
  | TRec fields -> List.exists (fun (_, t) -> occur_check v t) fields
  | _ -> false

let rec unify t1 t2 =
  match (t1, t2) with
  | (TVar r1, TVar r2) when r1 == r2 -> ()
  | (TVar r, t) | (t, TVar r) ->
      (match !r with
       | Link t' -> unify t' t
       | Free v ->
           if occur_check v t then raise (TypeError "Circular type")
           else r := Link t)
  | (TInt, TInt) | (TBool, TBool) | (TString, TString) -> ()
  | (TSet t1, TSet t2) -> unify t1 t2
  | (TFun (d1, r1), TFun (d2, r2)) -> unify d1 d2; unify r1 r2
  | (TRec fs1, TRec fs2) ->
      List.iter (fun (f, t2) ->
        match List.assoc_opt f fs1 with
        | Some t1 -> unify t1 t2
        | None -> raise (TypeError ("Field " ^ f ^ " not found"))
      ) fs2
  | _ -> raise (TypeError "Type mismatch")

type expr =
  | EInt of int
  | EBool of bool
  | EString of string
  | EVar of string
  | EIn of expr * expr
  | EUnion of expr * expr
  | ESubset of expr * expr
  | EFun of string * expr * expr
  | EApp of expr * expr
  | ERec of (string * expr) list
  | EField of expr * string
  | EAnd of expr * expr
  | EOr of expr * expr
  | ENot of expr
  | EImply of expr * expr
  | EPlus of expr * expr
  | ESet of expr list
  | ELess of expr * expr
  | EEq of expr * expr
  | EGreater of expr * expr
  | ELessEq of expr * expr
  | EGreaterEq of expr * expr

type action =
  | ANext of string * expr
  | AUnchanged of string list
  | AEnabled of action
  | AConj of action * action
  | AExpr of expr
  | AStutter of action * string list

type temporal =
  | TAlways of temporal
  | TEventually of temporal
  | TLeadsTo of temporal * temporal
  | TAction of action

type env = (string * typ) list

let rec primed_vars_action a =
  match a with
  | ANext (x, e) -> x :: primed_vars_expr e
  | AUnchanged _ -> []
  | AEnabled a -> primed_vars_action a
  | AConj (a1, a2) -> primed_vars_action a1 @ primed_vars_action a2
  | AExpr e -> primed_vars_expr e
  | AStutter (a, _) -> primed_vars_action a

and primed_vars_expr e =
  match e with
  | EInt _ | EBool _ | EString _ | EVar _ -> []
  | EIn (e1, e2) | EUnion (e1, e2) | ESubset (e1, e2) | EAnd (e1, e2) | EOr (e1, e2)
  | EImply (e1, e2) | EPlus (e1, e2) | ELess (e1, e2) | EEq (e1, e2)
  | EGreater (e1, e2) | ELessEq (e1, e2) | EGreaterEq (e1, e2) ->
      primed_vars_expr e1 @ primed_vars_expr e2
  | EFun (_, s, e) -> primed_vars_expr s @ primed_vars_expr e
  | EApp (f, e) -> primed_vars_expr f @ primed_vars_expr e
  | ERec fields -> List.concat (List.map (fun (_, e) -> primed_vars_expr e) fields)
  | EField (r, _) -> primed_vars_expr r
  | ESet es -> List.concat (List.map primed_vars_expr es)
  | ENot e -> primed_vars_expr e

let rec infer_expr (env : env) (e : expr) : typ =
  match e with
  | EInt _ -> TInt
  | EBool _ -> TBool
  | EString _ -> TString
  | EVar x -> (try List.assoc x env with Not_found -> raise (TypeError ("Unbound: " ^ x)))
  | EIn (e1, e2) ->
      let t1 = infer_expr env e1 in
      let t2 = infer_expr env e2 in
      let a = new_tvar () in
      unify t2 (TSet a); unify t1 a;
      TBool
  | EUnion (e1, e2) ->
      let t1 = infer_expr env e1 in
      let t2 = infer_expr env e2 in
      unify t1 t2;
      TSet t1
  | ESubset (e1, e2) ->
      let t1 = infer_expr env e1 in
      let t2 = infer_expr env e2 in
      let a = new_tvar () in
      unify t1 (TSet a); unify t2 (TSet a);
      TBool
  | EFun (x, s, e) ->
      let ts = infer_expr env s in
      let a = new_tvar () in
      unify ts (TSet a);
      let te = infer_expr ((x, a) :: env) e in
      TFun (a, te)
  | EApp (f, e) ->
      let tf = infer_expr env f in
      let te = infer_expr env e in
      let a = new_tvar () in
      unify tf (TFun (te, a));
      a
  | ERec fields ->
      TRec (List.map (fun (f, e) -> (f, infer_expr env e)) fields)
  | EField (r, f) ->
      let tr = infer_expr env r in
      let a = new_tvar () in
      unify tr (TRec [(f, a)]);
      a
  | EAnd (e1, e2) | EOr (e1, e2) | EImply (e1, e2) ->
      unify (infer_expr env e1) TBool;
      unify (infer_expr env e2) TBool;
      TBool
  | ENot e ->
      unify (infer_expr env e) TBool;
      TBool
  | EPlus (e1, e2) ->
      unify (infer_expr env e1) TInt;
      unify (infer_expr env e2) TInt;
      TInt
  | ESet es ->
      let ts = List.map (infer_expr env) es in
      (match ts with
       | [] -> TSet (new_tvar ())
       | t :: rest -> List.iter (unify t) rest; TSet t)
  | ELess (e1, e2) | EEq (e1, e2) | EGreater (e1, e2) | ELessEq (e1, e2) | EGreaterEq (e1, e2) ->
      let t1 = infer_expr env e1 in
      let t2 = infer_expr env e2 in
      (match e with
       | EEq _ -> unify t1 t2
       | _ -> unify t1 TInt; unify t2 TInt);
      TBool

let rec infer_action (env : env) (a : action) : typ =
  match a with
  | ANext (x, e) ->
      let tx = try List.assoc x env with Not_found -> raise (TypeError ("Unbound primed: " ^ x)) in
      unify (infer_expr env e) tx;
      TBool
  | AUnchanged vars ->
      List.iter (fun x ->
        if not (List.mem_assoc x env) then
          raise (TypeError ("Unbound variable in UNCHANGED: " ^ x))
      ) vars;
      TBool
  | AEnabled a ->
      unify (infer_action env a) TBool;
      TBool
  | AConj (a1, a2) ->
      unify (infer_action env a1) TBool;
      unify (infer_action env a2) TBool;
      TBool
  | AExpr e -> infer_expr env e
  | AStutter (a, vars) ->
      let t = infer_action env a in
      unify t TBool;
      let pv = primed_vars_action a in
      List.iter (fun x ->
        if not (List.mem x vars) then
          raise (TypeError ("Primed variable " ^ x ^ " not in action variables " ^ String.concat ", " vars))
      ) pv;
      TBool

let rec infer_temporal (env : env) (t : temporal) : typ =
  match t with
  | TAlways t | TEventually t ->
      unify (infer_temporal env t) TBool;
      TFormula
  | TLeadsTo (t1, t2) ->
      unify (infer_temporal env t1) TBool;
      unify (infer_temporal env t2) TBool;
      TFormula
  | TAction a -> infer_action env a

let rec string_of_typ = function
  | TInt -> "Int"
  | TBool -> "Bool"
  | TString -> "String"
  | TSet t -> "Set(" ^ string_of_typ t ^ ")"
  | TFun (t1, t2) -> "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
  | TRec fs -> "Rec[" ^ String.concat ", " (List.map (fun (f, t) -> f ^ ": " ^ string_of_typ t) fs) ^ "]"
  | TVar r -> (match !r with Free id -> "a" ^ string_of_int id | Link t -> string_of_typ t)
  | TFormula -> "Formula"

let test name infer_fn e ?(env = []) =
  try
    let t = infer_fn env e in
    Printf.printf "Test '%s': %s\n" name (string_of_typ t)
  with TypeError msg ->
    Printf.printf "Test '%s': Type error - %s\n" name msg

let () =
  let env = [("x", TInt); ("y", TInt)] in
  test "1 \\in {1, 2, 3}" infer_expr (EIn (EInt 1, ESet [EInt 1; EInt 2; EInt 3])) ~env:[];
  test "([x \\in {1, 2} |-> x + 1])[2]" infer_expr 
    (EApp (EFun ("x", ESet [EInt 1; EInt 2], EPlus (EVar "x", EInt 1)), EInt 2)) ~env:[];
  test "[a |-> 1, b |-> TRUE].b" infer_expr 
    (EField (ERec [("a", EInt 1); ("b", EBool true)], "b")) ~env:[];
  test "TRUE /\\ (1 \\in {1})" infer_expr 
    (EAnd (EBool true, EIn (EInt 1, ESet [EInt 1]))) ~env:[];
  test "1 + TRUE" infer_expr (EPlus (EInt 1, EBool true)) ~env:[];
  test "([x \\in {1, 2} |-> x + 1])[\"hello\"]" infer_expr 
    (EApp (EFun ("x", ESet [EInt 1; EInt 2], EPlus (EVar "x", EInt 1)), EString "hello")) ~env:[];
  test "[]TRUE" infer_temporal (TAlways (TAction (AExpr (EBool true)))) ~env:[];
  test "<>(1 \\in {1, 2})" infer_temporal 
    (TEventually (TAction (AExpr (EIn (EInt 1, ESet [EInt 1; EInt 2]))))) ~env:[];
  test "[]1" infer_temporal (TAlways (TAction (AExpr (EInt 1)))) ~env:[];
  test "<>(x + 1)" infer_temporal (TEventually (TAction (AExpr (EPlus (EVar "x", EInt 1))))) ~env;
  test "(1 \\in {1}) ~> TRUE" infer_temporal 
    (TLeadsTo (TAction (AExpr (EIn (EInt 1, ESet [EInt 1]))), TAction (AExpr (EBool true)))) ~env:[];
  test "1 ~> TRUE" infer_temporal 
    (TLeadsTo (TAction (AExpr (EInt 1)), TAction (AExpr (EBool true)))) ~env:[];
  test "x' = x + 1" infer_action (ANext ("x", EPlus (EVar "x", EInt 1))) ~env;
  test "[x' = x + 1]_x" infer_action (AStutter (ANext ("x", EPlus (EVar "x", EInt 1)), ["x"])) ~env;
  test "x < 2" infer_expr (ELess (EVar "x", EInt 2)) ~env;
  test "x = x'" infer_action (ANext ("x", EVar "x")) ~env;
  test "x < TRUE" infer_expr (ELess (EVar "x", EBool true)) ~env;
  test "[](x < 2)" infer_temporal (TAlways (TAction (AExpr (ELess (EVar "x", EInt 2))))) ~env;
  test "[x' = x + 1]_y" infer_action (AStutter (ANext ("x", EPlus (EVar "x", EInt 1)), ["y"])) ~env;
  test "[y' = x + 1]_{x,y}" infer_action (AStutter (ANext ("y", EPlus (EVar "x", EInt 1)), ["x"; "y"])) ~env;
  test "x > 2" infer_expr (EGreater (EVar "x", EInt 2)) ~env;
  test "x <= y" infer_expr (ELessEq (EVar "x", EVar "y")) ~env;
  test "x >= 1" infer_expr (EGreaterEq (EVar "x", EInt 1)) ~env;
  test "x > TRUE" infer_expr (EGreater (EVar "x", EBool true)) ~env;
  test "UNCHANGED <<x>>" infer_action (AUnchanged ["x"]) ~env;
  test "UNCHANGED <<x, y>>" infer_action (AUnchanged ["x"; "y"]) ~env;
  test "UNCHANGED <<z>>" infer_action (AUnchanged ["z"]) ~env:[];
  test "[UNCHANGED <<x>>]_x" infer_action (AStutter (AUnchanged ["x"], ["x"])) ~env;
  test "{1} \\subseteq {1, 2}" infer_expr (ESubset (ESet [EInt 1], ESet [EInt 1; EInt 2])) ~env:[];
  test "{1} \\subseteq 1" infer_expr (ESubset (ESet [EInt 1], EInt 1)) ~env:[];
  test "TRUE \\/ FALSE" infer_expr (EOr (EBool true, EBool false)) ~env:[];
  test "1 \\/ 2" infer_expr (EOr (EInt 1, EInt 2)) ~env:[];
  test "\\lnot TRUE" infer_expr (ENot (EBool true)) ~env:[];
  test "\\lnot 1" infer_expr (ENot (EInt 1)) ~env:[];
  test "TRUE => FALSE" infer_expr (EImply (EBool true, EBool false)) ~env:[];
  test "x => 1" infer_expr (EImply (EVar "x", EInt 1)) ~env;
  test "ENABLED (x' = x + 1)" infer_action (AEnabled (ANext ("x", EPlus (EVar "x", EInt 1)))) ~env
