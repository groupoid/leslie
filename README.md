Leslie: TLA+
============

Leslie is a formally specified language implemented in OCaml, featuring a statically typed subset
of TLA+ with primitive types (`Int`, `Bool`, `String`), basic arithmetic `+`, comprehensive comparison
operators `<`, `=`, `>`, `<=`, `>=`, logical connectives `/\`, `\/`, `\lnot`, `=>`,
set operations `\in`, `\cup`, `\subseteq`, literal sets), function constructs (application and lambda expressions),
record operations, a nondeterministic choice operator `CHOOSE`, a modular action system (next-state assignments,
stuttering, `UNCHANGED`, `ENABLED`, conjunction), and a recursive temporal logic system `[]`, `<>`, `~>`,
supported by a Hindley-Milner-style type inference mechanism, though it omits TLA+’s full
arithmetic suite, advanced set operations, quantifiers, fairness conditions, and module-level declarations.

Features
--------

* Core TLA+ constructs `sets`, `functions`, `records`.
* Temporal operators `[]`, `<>`, `~>`.
* Primed variables and actions with stuttering subscripts `[A]_v`.
* Comparison operators (`<`, `=`, `>`, `<=`, `>=`).
* `UNCHANGED`, `ENABLED`, `CHOOSE`.

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
