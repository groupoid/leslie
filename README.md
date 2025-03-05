Leslie: TLA+ Clone
==================

The TLA+ core:

```
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
