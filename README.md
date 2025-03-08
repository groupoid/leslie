Leslie: TLA+
============

<img src="https://leslie.groupoid.space/img/Leslie_Lamport.jpg">

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
  | EIntersect of expr * expr
  | EMinus of expr * expr
  | EPowerset of expr
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
  | EForall of string * expr * expr
  | EExists of string * expr * expr
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
Test '{1, 2} \cap {2, 3}': Set(Set(Int))
Test '{1, 2} \ {2}': Set(Set(Int))
Test 'POWERSET {1, 2}': Set(Set(Int))
Test '{1} \cap 1': Type error - Type mismatch
Test '\A x \in {1, 2}: x > 0': Bool
Test '\E x \in {1, 2}: x > 0': Bool
Test '\A x \in {1, 2}: x': Type error - Type mismatch
Test 'WF_x(x' = x + 1)': Formula
Test 'SF_{x,y}(y' = x)': Formula
```

Author
------

* Namdak Tonpa
