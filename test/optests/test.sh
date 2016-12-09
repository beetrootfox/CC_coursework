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

echo "Optimisation tests"

test "Constant folding test" ../test/optests/cfold.test ../test/optests/cfold.s ../test/optests/ans/cfold.ans
test "Constant propagation test" ../test/optests/cprop.test ../test/optests/cprop.s ../test/optests/ans/cprop.ans
test "Function inlining test" ../test/optests/finl.test ../test/optests/finl.s ../test/optests/ans/finl.ans
test "Lambda inlining test" ../test/optests/laminl.test ../test/optests/laminl.s ../test/optests/ans/laminl.ans
test "Pointer inlinging test" ../test/optests/pinl.test ../test/optests/pinl.s ../test/optests/ans/pinl.ans
test "Loop unrolling" ../test/optests/lunrl.test ../test/optests/lunrl.s ../test/optests/ans/lunrl.ans

echo "Optimisation tests DONE"


