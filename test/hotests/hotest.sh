function test {
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

echo "Higher order evaluation tests"

test "Variable capture test" ../test/hotests/capture.test ../test/hotests/capture.log ../test/hotests/ans/capture.ans
test "Function pointers/higher order functions test" ../test/hotests/fptr.test ../test/hotests/fptr.log ../test/hotests/ans/fptr.ans
test "Function application test" ../test/hotests/function.test ../test/hotests/function.log ../test/hotests/ans/function.ans
test "Lambda test" ../test/hotests/lambda.test ../test/hotests/lambda.log ../test/hotests/ans/lambda.ans
test "Let-binding test" ../test/hotests/let.test ../test/hotests/let.log ../test/hotests/ans/let.ans
test "Let-bound pointers test" ../test/hotests/lptr.test ../test/hotests/lptr.log ../test/hotests/ans/lptr.ans
test "New-bindning test" ../test/hotests/new.test ../test/hotests/new.log ../test/hotests/ans/new.ans
test "New-bound pointers" ../test/hotests/nptr.test ../test/hotests/nptr.log ../test/hotests/ans/nptr.ans
test "Recursion test" ../test/hotests/recfib.test ../test/hotests/recfib.log ../test/hotests/ans/recfib.ans
test "Scope/shadowing test" ../test/hotests/varscp.test ../test/hotests/varscp.log ../test/hotests/ans/varscp.ans 

echo "Tests DONE"
