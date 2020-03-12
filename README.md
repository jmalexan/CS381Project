# CS 381 Final Project for Winter 2020
Jonathan Alexander (alexajon), Julian Fortune (fortunej), Faaiq Waqar (waqarf)

## Language Overview
Welcome to Barely Functional-C! This is an Imperative Programming Language with the following features:

Basic data types and operations: There will be Integer, Boolean, and Floating point data types. Integer and Floats each individually support the 4 basic math arithmetic operations (*, /, +, -). The Boolean will have `and`, `or`, and `not` operators. There will be explicit casting between all three types.

Conditionals: There will be If statements. There will not be any other branching features, because if statements reinforce the key concepts of making decisions, and else if/else/switch statements are less precise and less explicit.

Recursion/loops: There will be looping structures in the program: While Loops, For Loops, and For each loops (iterate through the items in a list).

Variables/local names: Our language will allow binding and referencing variables. Scoping is based on functions.

Procedures/functions with arguments: There will be function structures in the Program that take in parameters fed by basis of execution in the function program itself, which also support return values

List/array data type and operations : There will be lists supported, based on the built in data types. The lists will support indexing, length/count, append, insert, and remove.

Static type system : A user attempting to write and use code in the programming language we have created would be using the GHCI to operate the language, the compiler is called before use of a function, or can be called to in order to check the correctness of the program, this would return one of certain types of error indicators ideally in a specific Expression as defined in the syntax. This will give users an idea of the correctness of their programs. The function is called ‘compile’ and takes in a ‘prog’ or string of commands

## Execution Instructions
1. Open Via GHCI
2. Load the file 'Program.hs'
3. Testing Good Input: `*Project> prog mainState`
4. Testing Bad Input: `*Project> prog badState`

## Todo:
- [!important] Develop Static Type system.
- @Julian Change list expressions to use Expr.
- Add context to errors using the fact we capture the Error text everywhere.
- Expand unit tests to cover more of the language.
- 💡 Spike: Research `do` notation and monads.
- 💡 Spike: Hinley Milner type checking
- 🤸🏼‍♀️ Stretch: Lists within lists.

## Done
- @Julian figure out if list functions have correct constructor parameter types (~String vs.~ Expr).
- Add simple `doctest` unit tests.
- Lists:
    - Appending (syntactic sugar/library level).
    - Range(start, end) function (library level).
    - Getting length (core).
    - Assigning expressions at index (syntactic sugar using delete and insert).
    - Deleting elements (~syntactic sugar using for each to make a copy~ core) (Julian thinks declaring variable names in syntactic sugar should be avoided, and the wouldn't be generic).
    - Iterate for each element (~syntactic sugar~ core).
    - Inserting expressions at index (core).
    - Getting elements (core).
- Learn how to use Map.
- Simplify language core by using nested types for constructors that take same number of args.
- Simplify language core by moving some features to synactic sugar.
- Develop the abstract syntax (grammar).
- Create semantic domain.
- Write example functions for testing.
- Move return parsing into `cmd` and change `prog` to check the result of cmd.
- Add custom exceptions (??) instead of using falling back on pattern match failure errors.