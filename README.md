# CC_coursework

#How to run
`make` will build the project. It may fail if you already have _build directory and/or test.native,
in this case run `make clean` and then `make`

`make tests` will run 14 parser tests:

        10 tests of basic functionality
        2 tests of bisection algorithms implementations
        2 tests of parse/syntax error messages
        
and 11 evaluation tests:

        9 tests of basic functionality
        2 tests of runtime error messages
        1 tests of fibonacci sequence computation
        
Output of parser tests is saved in `.log` files in `test` directory, evaluation tests have their output saved in `.elog` files.
Parser tests will produce `ERROR` output in response to a `Runtime error` or a `Parsing/Syntax error` and `OK` if tests pass.
Evaluation tests will produce `ERROR` in case of errors and `OK` if they pass, also, `WRONG EVALUATION`, if they are evaluated to an incorrect value. Most parser tests will fail with a `Runtime error` since they are run through the evaluator while containing `new/let` bindings and other unimplemented features.

#AST description
AST was not changed much. Notable changes:

    Added Empty case for expressions to handle Minus being applied to a single number (i.e. "-2") and other
    things such as using a function with 0 argumens (further details in the comments in the respective tests)

    Moved Not operator to a unary_opcode type and added expression case to handle unary operator application

#Syntax description --changed since assignment 1--
> C-like comments, start with "/*" end with "*/". Comments don't always work inside of function definitions
    preferably use them at the beggining of the program
    
> C-like if and while (i.e. "while(exp){exp}")

> Function application of form "(fun_exp<-arg_exp)"

> New and Let Ocaml-like, trailing `;` was removed.

> further notes about syntax as well as practical examples can be found in .test files in the test folder

#Evaluator
For now only basic logic and imperative features are supported, as well as `printint(x)` function.
Evaluator features explicit runtime errors that differentiate between failures happening due to a type mismatch, unbound variable dereferencing or the use of unimplemented features. Errors also provide information on the expected types, types that caused an error and unbound variable names.

Added New/Let bindings, Function application, Higher order function, Functors, Lambdas, Arrays, Printint support.
