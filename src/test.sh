bash ../test/test.sh
bash ../test/hotests/hotest.sh
bash ../test/optests/test.sh	
echo "power test"
./test.native < power > test.s
gcc -o test test.s
./test
echo "prog test"
./test.native < prog > test.s
gcc -o test test.s
./test
echo "convolution test"
./test.native < conv > test.s
gcc -o test test.s
./test

