[ -f ./curr_diff ] && rm curr_diff
cd ../../
./compile.sh
cd test_suite/codegen
cd ../
cp ../main .
cd codegen
gcc ../test.c -o test && ./test
rm test
