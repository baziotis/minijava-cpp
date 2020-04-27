[ -f ./curr_diff ] && rm curr_diff
cd ../
./compile.sh
cd test_suite/
gcc test_semantic.c -o test_semantic && ./test_semantic
rm test_semantic
# Note that we don't use codegen/test.sh here because
# that will recompile the compiler. We want to leave it
# in there in case someone wants to run codegen suite
# only and wants a clean compile.
# These things could be fixed by using the makefile
# but I just don't want to risk makes.
cd codegen/
gcc ./test_codegen.c -o test_codegen && ./test_codegen
cd ../
