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
	gcc -o test $3
	./test > $4
	if [ $? -eq 0 ]; then
		echo OK
	else
		echo ERROR
	fi
}		
	
echo "Will now run evaluation tests. Evaluation tests are .etest files and can be found in ../test/. Test results/error messages are saved in .elog files in the same directory. Answers to the tests are in ../test/ans/."


etest "Uniform Assignment test" ../test/uniformasg.etest ../test/uniformasg.s ../test/ans/asg.ans
etest "Uniform Binary operators test" ../test/uniformbinop.etest ../test/uniformbinop.s ../test/ans/binop.ans
etest "Uniform Dereference test" ../test/uniformderef.etest ../test/uniformderef.s ../test/ans/deref.ans
etest "Sequence evaluation test" ../test/seq.etest ../test/seq.s ../test/ans/seq.ans
etest "Conditional evaluation test" ../test/ifs.etest ../test/ifs.s ../test/ans/ifs.ans
etest "While loop evaluation test" ../test/while.etest ../test/while.s ../test/ans/while.ans
etest "Fibonacci sequence computation evaluation" ../test/fib.etest ../test/fib.s ../test/ans/fib.ans
etest "Advanced uniformity test" ../test/advuni.etest ../test/advuni.s ../test/ans/advuni.ans

echo "Evaluation tests DONE"
