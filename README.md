# CC_coursework

#How to run
`make parser` will build the project. It may fail if you already have _build directory and/or test.native
in this case run `make clean` and then `make parser`
`make tests` will run 14 tests:
        10 tests of basic functionality
        2 tests of bisection algorithms implementations
        2 tests of parse/syntax error messages
successful test outputs are stored in a .log file (outputs are string representations of AST)

#AST description
AST was not changed much. Notable changes:
    Added Empty case for expressions to handle Minus being applied to a single number (i.e. "-2") and other
    things such as using a function with 0 argumens (further details in the comments in the respective tests)

    Moved Not operator to a unary_opcode type and added expression case to handle unary operator application

#Syntax description
> C-like comments, start with "/*" end with "*/". Comments don't always work inside of function definitions
    preferably use them at the beggining of the program
> C-like if and while (i.e. "while(exp){exp}")
> Function application of form "(fun_exp<-arg_exp)"
> New and Let Ocaml-like but must always terminate with ";" (i.e. "new c = 1 in new a = c in new b = a in a;;;"). This was done to avoid shift/reduce conflicts (as of now language has none), may be improved later
> further notes about syntax as well as practical examples can be found in .test files in the test folder
