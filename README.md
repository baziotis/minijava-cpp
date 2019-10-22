# MiniJava Compiler in C++ (from scratch)

A compiler from MiniJava ([Grammar](http://cgi.di.uoa.gr/~thp06/project_files/minijava-new/minijava.html#prod4))
to [LLVM IR](https://llvm.org/docs/LangRef.html) written in (simple) C++.

Source code guide will come. The project is immature and there is a little of hackiness in the code (but just a little bit ;).

# Description

Currently, the compiler is in early development. As of writing this, lexing, parsing and type-checking are for the most
part done. But code generation (to LLVM IR) not. So **for now**, the compiler **works only as a type-checker**
for MiniJava and follows the convention "no news => good news". That is, you can pass a `.java` file in
the cmdline and if no message is issued, the the file was type-checked successfully.

# Usage
It is assumed that you have GCC (g++) installed. Generally, a C++ compiler is and will be the only dependency.

Compile: `$ ./compile.sh` (that will create an executable named `main`).

Run: `./main filename.java {-v} {-no-style} {-offsets}`

* `-v`: Verbose output. Using this argument, the compiler will print log info about pass 1 and 2 of type-checking.
* `-no-style`: By default, the error messages are styled (colored and bold). This argument turns styling off.
* `-offsets`: The compiler will print additional info for every class. For its fields, it will print their offset from the start of the class space. For methods, it will print their offset from the start of the virtual table. Except for methods that override a parent method, in which case their offset is the same as that of the parent method. _Note_: `bool` take 1 byte, `int` take 4 bytes and `int[]` take 8 bytes (because they're pointers under the hood). User-defined types take 8 bytes as well as they're also implemented as pointers under the hood (as in Java, entities with user-defined type are actually references, aka pointers, to objects of this type).

# Performance
There has been no serious effort to optimize the compiler. But, as of writing this, it **type-checks**
in the following speeds _on my PC_ (AMD Ryzen 7 1700, 16 GB RAM, nvme SSD).

| Build          | LOC / second    |
| :------------: | :-------------: |
| Optimized      | 1.05 million    |
| Unoptimized    | 700 thousands   |

This is relatively good but it could be better: (compiler internals follow):
- Pass 1 of type-checking it seems it can be eliminated by coupling it with the parsing stage.
- Codegen it seems it can be coupled with Pass 2 of type-checking.
- The memory consumption can be reduced. But I don't think that the _max_ memory consumption can be reduced.
  In any case, we can throw the memory away after compiling a method or a class. The problem is that we do have
  to first allocate for all the methods because of parsing. And we can't really an one-pass compiler because
  of order-independent declarations (i.e. `A` can be used before it's defined). If we could do this one pass,
  it would probably improve performance by a large margin.

These are some ideas off the top of my head but certainly, they will remain just ideas for some time
as the compiler focuses on simplicity and understandabilty first (but of course with more than reasonable performance).
