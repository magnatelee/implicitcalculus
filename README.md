# Implicit Calculus Interpreter

Implicit calculus[1] is the core calculus designed to provide the
foundation of implicit programming. For the details, please refer to
the draft.

This project includes an interpreter of the source language appeared
in the paper. The program written in the source language is first
translated into our implicit calculus which is eventually translated
into System F. Then, the interpreter evaluates the System F program
and prints out the result.

The biggest difference between the source language and the language
actually implemented is type inference: in the paper we assume that
the type inference will be done on programs, hence no type needs to be
annotated. In the implementation, however, we do not infer any types,
which means the user should specify the type for every let variable,
implicit lookup, and field access to records. Here is the modified
syntax of the source language:

E ::= x | u : T | ? T | ... (the rest are the same)

If you find any issues or bugs, please report to wonchan@stanford.edu.


[1] Bruno C. d. S. Oliveira, Tom Schrijvers, Wontae Choi, Wonchan Lee,
Kwangkeun Yi, Philip Wadler, The Implicit Calculus: A New Foundation
for Generic Programming, submitted to <i>ACM Transactions on
Programming Languages and Systems (TOPLAS)</i>




