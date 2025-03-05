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
  | EInt of int | EBool of bool | EString of string | EVar of string
  | EIn of expr * expr | EUnion of expr * expr | EFun of string * expr * expr
  | EApp of expr * expr | ERec of (string * expr) list | EField of expr * string
  | EAnd of expr * expr | EPlus of expr * expr | ESet of expr list
  | EAlways of expr | EEventually of expr | ELeadsTo of expr * expr
  | EPrime of string | EAction of expr * string list | ELess of expr * expr | EEq of expr * expr

type env = (string * typ) list

let rec primed_vars e =
  match e with
  | EInt _ | EBool _ | EString _ -> []
  | EVar _ -> []
  | EPrime x -> [x]
  | EIn (e1, e2) | EUnion (e1, e2) | EAnd (e1, e2) | EPlus (e1, e2) 
  | ELess (e1, e2) | EEq (e1, e2) | ELeadsTo (e1, e2) ->
      primed_vars e1 @ primed_vars e2
  | EFun (_, s, e) -> primed_vars s @ primed_vars e
  | EApp (f, e) -> primed_vars f @ primed_vars e
  | ERec fields -> List.concat (List.map (fun (_, e) -> primed_vars e) fields)
  | EField (r, _) -> primed_vars r
  | ESet es -> List.concat (List.map primed_vars es)
  | EAlways e | EEventually e | EAction (e, _) -> primed_vars e

let rec infer (env : env) (e : expr) : typ =
  match e with
  | EInt _ -> TInt
  | EBool _ -> TBool
  | EString _ -> TString
  | EVar x -> (try List.assoc x env with Not_found -> raise (TypeError ("Unbound: " ^ x)))
  | EIn (e1, e2) ->
      let t1 = infer env e1 in
      let t2 = infer env e2 in
      let a = new_tvar () in
      unify t2 (TSet a); unify t1 a;
      TBool
  | EUnion (e1, e2) ->
      let t1 = infer env e1 in
      let t2 = infer env e2 in
      unify t1 t2;
      TSet t1
  | EFun (x, s, e) ->
      let ts = infer env s in
      let a = new_tvar () in
      unify ts (TSet a);
      let te = infer ((x, a) :: env) e in
      TFun (a, te)
  | EApp (f, e) ->
      let tf = infer env f in
      let te = infer env e in
      let a = new_tvar () in
      unify tf (TFun (te, a));
      a
  | ERec fields ->
      TRec (List.map (fun (f, e) -> (f, infer env e)) fields)
  | EField (r, f) ->
      let tr = infer env r in
      let a = new_tvar () in
      unify tr (TRec [(f, a)]);
      a
  | EAnd (e1, e2) ->
      unify (infer env e1) TBool;
      unify (infer env e2) TBool;
      TBool
  | EPlus (e1, e2) ->
      unify (infer env e1) TInt;
      unify (infer env e2) TInt;
      TInt
  | ESet es ->
      let ts = List.map (infer env) es in
      (match ts with
       | [] -> TSet (new_tvar ())
       | t :: rest -> List.iter (unify t) rest; TSet t)
  | EAlways e ->
      unify (infer env e) TBool;
      TFormula
  | EEventually e ->
      unify (infer env e) TBool;
      TFormula
  | ELeadsTo (p, q) ->
      unify (infer env p) TBool;
      unify (infer env q) TBool;
      TFormula
  | EPrime x ->
      (try List.assoc x env with Not_found -> raise (TypeError ("Unbound primed: " ^ x)))
  | EAction (e, vars) ->
      let t = infer env e in
      unify t TBool;
      let pv = primed_vars e in
      List.iter (fun x ->
        if not (List.mem x vars) then
          raise (TypeError ("Primed variable " ^ x ^ " not in action variables " ^ String.concat ", " vars))
      ) pv;
      TBool
  | ELess (e1, e2) ->
      unify (infer env e1) TInt;
      unify (infer env e2) TInt;
      TBool
  | EEq (e1, e2) ->
      let t1 = infer env e1 in
      let t2 = infer env e2 in
      unify t1 t2;
      TBool

let rec string_of_typ = function
  | TInt -> "Int"
  | TBool -> "Bool"
  | TString -> "String"
  | TSet t -> "Set(" ^ string_of_typ t ^ ")"
  | TFun (t1, t2) -> "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
  | TRec fs -> "Rec[" ^ String.concat ", " (List.map (fun (f, t) -> f ^ ": " ^ string_of_typ t) fs) ^ "]"
  | TVar r -> (match !r with Free id -> "a" ^ string_of_int id | Link t -> string_of_typ t)
  | TFormula -> "Formula"

let test_expr name e ?(env = []) =
  try
    let t = infer env e in
    Printf.printf "Test '%s': %s\n" name (string_of_typ t)
  with TypeError msg ->
    Printf.printf "Test '%s': Type error - %s\n" name msg

let () =
  let env = [("x", TInt); ("y", TInt)] in
  test_expr "1 \\in {1, 2, 3}" (EIn (EInt 1, ESet [EInt 1; EInt 2; EInt 3])) ~env:[];
  test_expr "([x \\in {1, 2} |-> x + 1])[2]" 
    (EApp (EFun ("x", ESet [EInt 1; EInt 2], EPlus (EVar "x", EInt 1)), EInt 2)) ~env:[];
  test_expr "[a |-> 1, b |-> TRUE].b" 
    (EField (ERec [("a", EInt 1); ("b", EBool true)], "b")) ~env:[];
  test_expr "TRUE /\\ (1 \\in {1})" 
    (EAnd (EBool true, EIn (EInt 1, ESet [EInt 1]))) ~env:[];
  test_expr "1 + TRUE" (EPlus (EInt 1, EBool true)) ~env:[];
  test_expr "([x \\in {1, 2} |-> x + 1])[\"hello\"]" 
    (EApp (EFun ("x", ESet [EInt 1; EInt 2], EPlus (EVar "x", EInt 1)), EString "hello")) ~env:[];
  test_expr "[]TRUE" (EAlways (EBool true)) ~env:[];
  test_expr "<>(1 \\in {1, 2})" (EEventually (EIn (EInt 1, ESet [EInt 1; EInt 2]))) ~env:[];
  test_expr "[]1" (EAlways (EInt 1)) ~env:[];
  test_expr "<>(x + 1)" (EEventually (EPlus (EVar "x", EInt 1))) ~env;
  test_expr "(1 \\in {1}) ~> TRUE" 
    (ELeadsTo (EIn (EInt 1, ESet [EInt 1]), EBool true)) ~env:[];
  test_expr "1 ~> TRUE" (ELeadsTo (EInt 1, EBool true)) ~env:[];
  test_expr "x' = x + 1" (EEq (EPrime "x", EPlus (EVar "x", EInt 1))) ~env;
  test_expr "[x' = x + 1]_x" (EAction (EEq (EPrime "x", EPlus (EVar "x", EInt 1)), ["x"])) ~env;
  test_expr "x < 2" (ELess (EVar "x", EInt 2)) ~env;
  test_expr "x = x'" (EEq (EVar "x", EPrime "x")) ~env;
  test_expr "x < TRUE" (ELess (EVar "x", EBool true)) ~env;
  test_expr "[](x < 2)" (EAlways (ELess (EVar "x", EInt 2))) ~env;
  test_expr "[x' = x + 1]_y" (EAction (EEq (EPrime "x", EPlus (EVar "x", EInt 1)), ["y"])) ~env;
  test_expr "[y' = x + 1]_{x,y}" (EAction (EEq (EPrime "y", EPlus (EVar "x", EInt 1)), ["x"; "y"])) ~env
