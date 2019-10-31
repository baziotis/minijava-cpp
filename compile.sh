# Go make a coffee, we're about to compile C++.
g++ -std=c++17 main.cpp lex.cpp tokens.cpp str_intern.cpp error.cpp ast.cpp parse.cpp debug_print.cpp typecheck.cpp alloc.cpp llvm.cpp -ggdb -o main -O3
