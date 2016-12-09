function test {
	echo $1
	./test.native -o < $2 &> $3
	gcc -o test $3
	./test > $4
	if [ $? -eq 0 ]; then
		echo OK
	else	
		echo ERROR
	fi
}

echo "Higher order evaluation tests"

test "Variable capture test" ../test/hotests/capture.test ../test/hotests/capture.s ../test/hotests/ans/capture.ans
test "Function pointers/higher order functions test" ../test/hotests/fptr.test ../test/hotests/fptr.s ../test/hotests/ans/fptr.ans
test "Function application test" ../test/hotests/function.test ../test/hotests/function.s ../test/hotests/ans/function.ans
test "Let-binding test" ../test/hotests/let.test ../test/hotests/let.s ../test/hotests/ans/let.ans
test "Let-bound pointers test" ../test/hotests/lptr.test ../test/hotests/lptr.s ../test/hotests/ans/lptr.ans
test "New-bindning test" ../test/hotests/new.test ../test/hotests/new.s ../test/hotests/ans/new.ans
test "New-bound pointers" ../test/hotests/nptr.test ../test/hotests/nptr.s ../test/hotests/ans/nptr.ans
test "Recursion test" ../test/hotests/recfib.test ../test/hotests/recfib.s ../test/hotests/ans/recfib.ans

echo "Tests DONE"
