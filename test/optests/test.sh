function test {
        echo $1
        ./test.native -o < $2 &> $3
        if [ $? -eq 0 ]; then
                echo "$(diff $3 $4)"
        else    
                echo ERROR
        fi
}

echo "Optimisation tests"

test "Constant folding test" ../test/optests/cfold.test ../test/optests/cfold.log ../test/optests/ans/cfold.ans
test "Constant propagation test" ../test/optests/cprop.test ../test/optests/cprop.log ../test/optests/ans/cprop.ans
test "Function inlining test" ../test/optests/finl.test ../test/optests/finl.log ../test/optests/ans/finl.ans
test "Lambda inlining test" ../test/optests/laminl.test ../test/optests/laminl.log ../test/optests/ans/laminl.ans
test "Pointer inlinging test" ../test/optests/pinl.test ../test/optests/pinl.log ../test/optests/ans/pinl.ans
test "Recursive function inlining (or lack of thereof) test" ../test/optests/recfinl.test ../test/optests/recfinl.log ../test/optests/ans/recfinl.ans
test "Loop unrolling" ../test/optests/lunrl.test ../test/optests/lunrl.log ../test/optests/ans/lunrl.ans

echo "Optimisation tests DONE"


