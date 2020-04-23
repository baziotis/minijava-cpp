[ -f ./curr_diff ] && rm curr_diff
cd ../
./compile.sh
cd test_suite/
gcc test_semantic.c -o test_semantic && ./test_semantic
rm test_semantic
# cd codegen/
# ./test.sh
# cd ../
