# CSCI 5511 programming assignment

```
CSCI 5511
programming assignment

Wei-Wei Wu
wuxx1045
5052973

12/05/17
```

## missionaries and cannibals problem

run `clisp cannibals.lisp`

OR in clisp interpreter:

```
(load 'cannbals.lisp)

(setq *starting-state* '(15 15 1)) ;; or '(24 24 1)
(run-cannibals *starting-state*)
```

## 9 puzzles problem

run `clisp puzzles.lisp`

OR in clisp interpreter:

```
(load 'puzzles.lisp)

(setq *starting-state* '(0 1 3 4 2 5 7 8 6))
(run-puzzles *starting-state*)
(format t "Number of nodes expanded: ~d~%" *counter*)
```
