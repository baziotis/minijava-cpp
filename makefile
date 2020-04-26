CC = g++
CFLAGS = -ggdb -std=c++17 -O3

bin: alloc.o ast.o debug_print.o error.o lex.o llvm.o main.o parse.o str_intern.o tokens.o typecheck.o 
	$(CC) $(CFLAGS) alloc.o ast.o debug_print.o error.o lex.o llvm.o main.o parse.o str_intern.o tokens.o typecheck.o -o main 

alloc.o : alloc.cpp alloc.h common.h 
	$(CC) $(CFLAGS) -c alloc.cpp 

ast.o : ast.cpp ast.h debug_print.h 
	$(CC) $(CFLAGS) -c ast.cpp 

debug_print.o : debug_print.cpp common.h debug_print.h error.h 
	$(CC) $(CFLAGS) -c debug_print.cpp 

error.o : error.cpp common.h tokens.h ast.h typecheck.h 
	$(CC) $(CFLAGS) -c error.cpp 

lex.o : lex.cpp common.h error.h str_intern.h tokens.h 
	$(CC) $(CFLAGS) -c lex.cpp 

llvm.o : llvm.cpp buf.h common.h debug_print.h llvm.h typecheck.h 
	$(CC) $(CFLAGS) -c llvm.cpp 

main.o : main.cpp ast.h common.h parse.h typecheck.h 
	$(CC) $(CFLAGS) -c main.cpp 

parse.o : parse.cpp ast.h debug_print.h error.h lex.h str_intern.h tokens.h 
	$(CC) $(CFLAGS) -c parse.cpp 

str_intern.o : str_intern.cpp alloc.h common.h str_intern.h 
	$(CC) $(CFLAGS) -c str_intern.cpp 

tokens.o : tokens.cpp common.h tokens.h 
	$(CC) $(CFLAGS) -c tokens.cpp 

typecheck.o : typecheck.cpp ast.h alloc.h array.h common.h debug_print.h error.h hash_table.h llvm.h str_intern.h 
	$(CC) $(CFLAGS) -c typecheck.cpp 

.PHONY : clear

clear :
	rm -f main alloc.o ast.o debug_print.o error.o lex.o llvm.o main.o parse.o str_intern.o test.o tokens.o typecheck.o 


#Generated with makefile generator: https://github.com/GeorgeLS/Makefile-Generator/blob/master/mfbuilder.c
