#!/bin/bash

./test.native < ../test/const.test > ../test/const.log
echo "running const.test, if failed see exception message above"
./test.native < ../test/id.test > ../test/id.log
echo "running id.test, if failed see exception message above"
./test.native < ../test/asg.test > ../test/asg.log
echo "running asg.test, if failed see exception message above"
./test.native < ../test/binop.test > ../test/binop.log
echo "running binop.test, if failed see exception message above"
./test.native < ../test/deref.test > ../test/deref.log
echo "running deref.test, if failed see exception message above"
./test.native < ../test/funap.test > ../test/funap.log
echo "running funap.test, if failed see exception message above"
./test.native < ../test/ifs.test > ../test/ifs.log
echo "running ifs.test, if failed see exception message above"
./test.native < ../test/newlet.test > ../test/newlet.log
echo "running newlet.test, if failed see exception message above"
./test.native < ../test/unop.test > ../test/unop.log
echo "running unop.test, if failed see exception message above"
./test.native < ../test/while.test > ../test/while.log
echo "running while.test, if failed see exception message above"
./test.native < ../test/it_bisection.test > ../test/it_bisection.log
echo "running it_bisection.test, if failed see exception message above"
./test.native < ../test/rec_bisection.test > ../test/rec_bisection.log
echo "running rec_bisection.test, if failed see exception message above"
./test.native < ../test/perror.test
echo "running parsing error test, see exception above"
./test.native < ../test/serror.test
echo "running syntax error test, see exception above"
