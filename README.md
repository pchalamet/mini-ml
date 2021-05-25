# MiniML

MiniML is a parser implementation of an ML-dialect with indentation-sensitive grammar.

This is done just to learn and fun :-) Do whatever you want with this.

# Syntax

## Module declaration
Mandatory and must be the fist thing declared in a file.

```
module toto
```

## Binding

```
let a = 42
```
Support expressions evaluation as well.

## Function call

### Tuple style
```
toto(42, 666)
```

### partial-call style
```
toto 42 666
```

## Pattern matching
```
match <expression> with
| <case> => ...
| _ ==> ...
```
