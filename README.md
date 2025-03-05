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

Tests
-----

```
Test '1 \in {1, 2, 3}': Bool
Test '([x \in {1, 2} |-> x + 1])[2]': Int
Test '[a |-> 1, b |-> TRUE].b': Bool
Test 'TRUE /\ (1 \in {1})': Bool
Test '1 + TRUE': Type error - Type mismatch
Test '([x \in {1, 2} |-> x + 1])["hello"]': Type error - Type mismatch
Test '[]TRUE': Formula
Test '<>(1 \in {1, 2})': Formula
Test '[]1': Type error - Type mismatch
Test '<>(x + 1)': Type error - Type mismatch
Test '(1 \in {1}) ~> TRUE': Formula
Test '1 ~> TRUE': Type error - Type mismatch
Test 'x' = x + 1': Bool
Test '[x' = x + 1]_x': Bool
Test 'x < 2': Bool
Test 'x = x'': Bool
Test 'x < TRUE': Type error - Type mismatch
Test '[](x < 2)': Formula
Test '[x' = x + 1]_y': Type error - Primed variable x not in action variables y
Test '[y' = x + 1]_{x,y}': Bool
Test 'x > 2': Bool
Test 'x <= y': Bool
Test 'x >= 1': Bool
Test 'x > TRUE': Type error - Type mismatch
Test 'UNCHANGED <<x>>': Bool
Test 'UNCHANGED <<x, y>>': Bool
Test 'UNCHANGED <<z>>': Type error - Unbound variable in UNCHANGED: z
Test '[UNCHANGED <<x>>]_x': Bool
Test '{1} \subseteq {1, 2}': Bool
Test '{1} \subseteq 1': Type error - Type mismatch
Test 'TRUE \/ FALSE': Bool
Test '1 \/ 2': Type error - Type mismatch
Test '\lnot TRUE': Bool
Test '\lnot 1': Type error - Type mismatch
Test 'TRUE => FALSE': Bool
Test 'x => 1': Type error - Type mismatch
Test 'ENABLED (x' = x + 1)': Bool
Test 'CHOOSE x \in {1, 2}: x > 0': Int
Test 'CHOOSE x \in {1, 2}: TRUE': Int
Test 'CHOOSE x \in {1, 2}: x': Type error - Type mismatch
Test 'CHOOSE x \in 1: x > 0': Type error - Type mismatch
```

Credits
-------

* Namdak Tonpa
