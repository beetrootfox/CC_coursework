# CC_coursework

#How to run
`make` will build the project. It may fail if you already have _build directory and/or test.native,
in this case run `make clean` and then `make`

`make` will build the project. It may fail if you already have `_build` directory and/or `test.native`, in this case run `make clean` and then `make`

Do `bash test.sh` from the source folder to run full set of regression tests. Evaluation tests can be found along the path `/test/` under the `.etest` extension, not all of the features of the evaluator are included into the final build, consult `test.sh` file in the same folder to see which exact files are being executed (test script itself provides some description of what is being tested). Outpus can be found under `/test/ans`. Another set of evaluation tests is under `/test/hotests` (with outputs written to the `/test/hotests/ans` folder), it checks some of the more advanced features such as recursion and function pointers (arrays were not included in the final build). Optimisation tests are under `/test/optests` (folder has the same structure as the hotests). Finally, some assembly-targeted tests as well as implementations of convolution and fizzbuzz algorithms can be found in the source folder (`/src/`). Output of these tests is printed directly to the command line during `/src/test.sh` execution.

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

Now have optimisation, woah!
