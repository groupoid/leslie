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
  | EChoose of string * expr * expr
```

```
type action =
  | ANext of string * expr
  | AUnchanged of string list
  | AEnabled of action
  | AConj of action * action
  | AExpr of expr
  | AStutter of action * string list
```

```
type temporal =
  | TAlways of temporal
  | TEventually of temporal
  | TLeadsTo of temporal * temporal
  | TAction of action
```

Credits
-------

* Namdak Tonpa
