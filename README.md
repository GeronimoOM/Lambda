# Typeless Lambda-Calculus

This is a pure Haskell implementation of typeless lambda-calculus.
The project is a console REPL application, which evalutes submitted lambda-expressions.

It supports recursive definitions, variable assignment, reading assignments from files, and evaluation in both standard and nameless (Brujin's) representation.

Provides ready definitions for
* data types: integer, boolean, pair
* mathematical and logical operators
* control flow statements: if-then-else, let-in

## Usage

Console accepts commands in [:_command_ [_command argument_]] _expression_ format
### List of commands
* eval - standard expression evaluation, prints result
* list - prints expression after each reduction
* steps - prints result and number of reduction performed
* eff - evaluation using nameless terms (erases names, but has much better performance)
* set _var = expr_ - assign expression to variable
* load _filename_ - load definitions from file
* ctx - prints all current variable assignments
* rmv _var_ - remove variable assignment
* clr - remove all assignments
* quit - terminate

## Examples
* Basic lambda-expressions
```
\x -> x (x x)
\x y -> y x
```
* Using defined operators, data types, and control flow statements
```
if (2 == 3 | false) then (17 + 4) else (2 * 3)
let flip a = ((snd a) (fst a)) in flip (1, 5)
let fact i = if i? then 1 else i * fact i-- in fact 5
```
* Using commands
```
:list 2++
:eff 15 div 4
:set gcd n m = if n? then m else let s = sort (n, m) in gcd ((snd s) % (fst s)) (fst s)
```
## Sources

* Rojas, R. (2015, March 28). A Tutorial Introduction to the Lambda Calculus. Retrieved from https://arxiv.org/abs/1503.09060
* Pierce, B. C. (2002, January 03). Types and Programming Languages. Retrieved from https://mitpress.mit.edu/books/types-and-programming-languages
