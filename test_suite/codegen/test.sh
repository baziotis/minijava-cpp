[ -f ./curr_diff ] && rm curr_diff
cd ../../
./compile.sh
cd test_suite/codegen
gcc ./test_codegen.c -o test_codegen && ./test_codegen
rm test_codegen
