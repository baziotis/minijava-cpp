[ -f ./curr_diff ] && rm curr_diff
cd ../
./compile.sh
cd test_suite/
gcc test.c -o test && ./test
rm test
