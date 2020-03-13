# CS 381 Final Project for Winter 2020
Jonathan Alexander (alexajon), Julian Fortune (fortunej), Faaiq Waqar (waqarf)

## Language Overview
Welcome to Barely Functional-C, an *imperitive* language. The goals of this language were to be easy to use in unique ways, including a reduction in side effects via functions, and function level scope.  On top of the core features outlined in the project spec, we implemented Lists, Strings, and a Static Type Checker.

## Execution Instructions
Open via GHCI 'Examples.hs'

### Good Examples

#### Quicksort Implementation

```
*Examples> compile quickSort
```

#### General Valid Program

```
*Examples> compile goodProg
```

#### Factorial Implementation

```
*Examples> compile factorial
```

### Bad Examples

#### General Error Example

```
*Examples> compile badProg
```

#### Runtime Error Examples

```
*Examples> compile badCastValue
```

#### Type Error Examples

```
*Examples> compile badFuncNoExist
*Examples> compile badOperTypes
*Examples> compile badCastType
*Examples> compile badVarRef
*Examples> compile badElement
*Examples> compile badFuncRef
*Examples> compile badFuncRedeclare
*Examples> compile badFuncArgs
*Examples> compile badIfCondition
*Examples> compile badWhileCondition
*Examples> compile badForEachIterator
*Examples> compile badInsertIndex
*Examples> compile badDeleteIndex
*Examples> compile badVarRedeclaration
*Examples> compile badFuncArgTypes
*Examples> compile badFuncReturnType
```