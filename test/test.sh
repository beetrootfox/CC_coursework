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
	
echo "Will now run evaluation tests. Evaluation tests are .etest files and can be found in ../test/. Test results/error messages are saved in .elog files in the same directory. Answers to the tests are in ../test/ans/."


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
