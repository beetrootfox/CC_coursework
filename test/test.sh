function test {
	echo $1
	./test.native < $2 &> $3
	if [ $? -eq 0 ]; then
        echo OK
	else
        echo ERROR
	fi
}

function etest {
	echo $1
	./test.native < $2 &> $3
	if [ $? -eq 0 ]; then
		if [ "$(diff $3 $4)" = "" ]; then
		echo OK
		else
		echo WRONG EVALUATION
		fi
	else
		echo ERROR
	fi
}		
	
echo "Will now run tests. Test output can be found in .log files in ../test/."
echo "Parsing tests BEGIN"
test "Constant parsing" ../test/const.test ../test/const.log
test "Identifier parsing" ../test/id.test ../test/id.log
test "Assignment parsing" ../test/asg.test ../test/asg.log
test "Binary operator parsing" ../test/binop.test ../test/binop.log
test "Dereference parsing" ../test/deref.test ../test/deref.log
test "Function application parsing" ../test/funap.test ../test/funap.log
test "Conditionals parsing" ../test/ifs.test ../test/ifs.log
test "New/Let parsing" ../test/newlet.test ../test/newlet.log
test "Unary operator parsing" ../test/unop.test ../test/unop.log
test "While loop parsing" ../test/while.test ../test/while.log
test "Iterative bisection parsing" ../test/it_bisection.test ../test/it_bisection.log
test "Recursive bisection parsing" ../test/rec_bisection.test ../test/rec_bisection.log
test "Parsing error example" ../test/perror.test ../test/perror.log
test "Syntactic error example" ../test/serror.test ../test/serror.log

echo "Parsing tests DONE"
echo "Will now run evaluation tests. Evaluation tests are .etest files and can be found in ../test/. Test results/error messages are saved in .elog files in the same directory. Answers to the tests are in ../test/ans/."
echo "Evaluation tests BEGIN"

etest "Uniform Assignment test" ../test/uniformasg.etest ../test/uniformasg.elog ../test/ans/asg.ans
etest "Uniform Binary operators test" ../test/uniformbinop.etest ../test/uniformbinop.elog ../test/ans/binop.ans
etest "Uniform Dereference test" ../test/uniformderef.etest ../test/uniformderef.elog ../test/ans/deref.ans
etest "Uniform Unary operator test" ../test/uniformunop.etest ../test/uniformunop.elog ../test/ans/unop.ans
etest "Sequence evaluation test" ../test/seq.etest ../test/seq.elog ../test/ans/seq.ans
etest "Conditional evaluation test" ../test/ifs.etest ../test/ifs.elog ../test/ans/ifs.ans
etest "While loop evaluation test" ../test/while.etest ../test/while.elog ../test/ans/while.ans
etest "Fibonacci sequence computation evaluation" ../test/fib.etest ../test/fib.elog ../test/ans/fib.ans
etest "Advanced uniformity test" ../test/advuni.etest ../test/advuni.elog ../test/ans/advuni.ans
test "Runtime error, wrong type" ../test/runtimeerr1.etest ../test/runtimeerr1.elog
test "Runtime error, undefined variable" ../test/runtimeerr2.etest ../test/runtimeerr2.elog

echo "Evaluation tests DONE"
