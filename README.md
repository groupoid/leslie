Leslie: TLA+
============

Features
--------

* Core TLA+ constructs `sets`, `functions`, `records`.
* Temporal operators `[]`, `<>`, `~>`.
* Primed variables and actions with stuttering subscripts `[A]_v`.
* Comparison operators (`<`, `=`, `>`, `<=`, `>=`).
* `UNCHANGED v` with proper variable binding checks.

Syntax
------

```
type typ =
  | TInt | TBool | TString | TVar of tvar ref | TFormula
  | TSet of typ | TFun of typ * typ | TRec of (string * typ) list
and tvar = Free of int | Link of typ

type expr =
  | EInt of int | EBool of bool | EString of string | EVar of string
  | EIn of expr * expr | EUnion of expr * expr | EFun of string * expr * expr
  | EApp of expr * expr | ERec of (string * expr) list | EField of expr * string
  | EAnd of expr * expr | EPlus of expr * expr | ESet of expr list
  | EAlways of expr | EEventually of expr | ELeadsTo of expr * expr

```

Credits
-------

* Namdak Tonpa
