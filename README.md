# MiniJava Compiler in C++ (from scratch)

A compiler from MiniJava ([Grammar](http://cgi.di.uoa.gr/~thp06/project_files/minijava-new/minijava.html#prod4)) to [LLVM IR](https://llvm.org/docs/LangRef.html) written in (simple) C++. To the best of my knowledge, **this is the fastest Minijava (to LLVM IR) compiler.**

# Description

Currently, the compiler is in early development. As of writing this, lexing, parsing and type-checking and codegen are for the most part done. A short usage guide follows and then a source code guide.

# Usage
It is assumed that you have GCC (g++) installed. Generally, a C++ compiler is and will be the only dependency.

Compile: `$ ./compile.sh` (that will create an executable named `main`).

In essence, the compiler can be used for two purposes: Either to only type-check and MiniJava (`.java`) source file or to also generate LLVM IR.

Run: `./main filename.java {-v} {-no-style} {-offsets}`

* `-v`: Verbose output. Using this argument, the compiler will print log info about pass 1 and 2 of type-checking.
* `-no-style`: By default, the error messages are styled (colored and bold). This argument turns styling off.
* `-offsets`: The compiler will print additional info for every class. For its fields, it will print their offset from the start of the class space. For methods, it will print their offset from the start of the virtual table. Except for methods that override a parent method, in which case their offset is the same as that of the parent method. _Note_: `bool` take 1 byte, `int` take 4 bytes and `int[]` take 8 bytes (because they're pointers under the hood). User-defined types take 8 bytes as well as they're also implemented as pointers under the hood (as in Java, entities with user-defined type are actually references, aka pointers, to objects of this type).
* `-codegen`: The compiler will generate LLVM IR.

# Performance
There has been no serious effort to optimize the compiler. But, as of writing this, it **type-checks**
in the following speeds (CPU: Intel Core i5-6500 @ 3.2ghz, RAM: 16G ).

| Build          | LOC / second    |
| :------------: | :-------------: |
| Optimized      | 1.05 million    |

This is relatively good but it could be better: (compiler internals follow):
- The memory consumption can be reduced. But I don't think that the _max_ memory consumption can be reduced.  In any case, we can throw the memory away after compiling a method or a class. The problem is that we do have  to first allocate for all the methods because of parsing. And we can't really make an one-pass compiler because   of order-independent declarations (i.e. `A` can be used before it's defined). If we could do this one pass,  it would probably improve performance by a large margin.

These are some ideas off the top of my head but certainly, they will remain just ideas for some time
as the compiler focuses on simplicity first (but of course with more than reasonable performance).

The codegen performance is a little worse, mainly because the compiler has to print a lot of code. The codegen part without the printing reaches about the same performance (Note that codegen and type-checking are done in 1 pass).


# Motivation
This is an side-project of mine with the purpose to experiment in different things regarding compilers. Basically, I wanted a simple language that was reasonably known and useful and also that had a couple of important characteristics.

## Why MiniJava ?

### Simplicity
It has a simple [grammar](http://cgi.di.uoa.gr/~thp06/project_files/minijava-new/minijava.html#prod4), simple type-checking rules and simple code generation.


# Source Guide

I hope that this will help people understand the code and maybe learn something from it.
A small warning: The project is still immature and the code is not amazingly good. I have only left out from refactoring pieces that are easily manageable.

## Prerequisites
I don't expect the reader to know very much about compilers but I expect them to have some basic knowledge. That is, it's good to: 
* know what the terms lexing, parsing, type-checking, code-generation and IR mean.
* know why do we follow the order lexing -> parsing -> type-checking -> code generation.
* be able to read a BNF Grammar.
* know what an AST is.
* know what a recursive-descent parser is.
* know basic LLVM IR.

## Lexing

Lexing is supposed to be the easy part and it basically is. The tricky part in lexers
is to make them really robust. The parser depends on the lexer, the type-checker on the parser and the code generator on the type-checker. So, everything directly or indirectly depends on the lexer working flawlessly.
What's more, the hard work in a compiler in terms of algorithms, code architecture etc. are not on the lexer. But when you try to think this hard work through, you don't want to be fighting the lexer at the same time.
**Bottom-line, you want the lexer to be fast and flawless.**

The work is done in `lex.cpp`. To start working with the lexer, you first have to call `lexer_init()`.
It takes the `input` for a file (in one big contiguous NUL-terminated buffer) and the name of the file.
It then initializes `charmap` and `digit_from_char`. These are 2 lookup tables for characters which help us get info for a character. `charmap` tells us things like if the character can appear in the middle of an identifier or if it could appear in a hex literal. `digit_from_char` gives us for a character digit, its numeric value. And then we initialize `input` (the global) and call `next_token()` to put the first token in the global `token`.

### next_token()
This is where all the work is done. To do lexing, we mainly work with 3 global variables. Two are exported.
- `loc` is the current location in the file. This is essentially an `int`. Since the compiler currently works with only one file, the line number suffices since all error-reporting needs the line number and the filename (the filename is saved as a global in `main.cpp` but let's not complicate things further).
- `token` is the last token found and it's the most important thing in lexing. Anyone who uses the lexer just calls `next_token()` and that puts the next token in `token`. This is a really convenient design but it has its drawbacks. Mainly a global is always a global and is almost always bad. If multi-threading is used, this will be a problem. This compiler indeed started with the purpose to be multi-threaded so I considered it. But changing the interface from putting the token to a global to returning is pretty easy and for the time being, the global is way better.
- `input` is just a pointer to the next character in the input. When it is `\0`, the input has ended. It's used only internally.

Now, to the inner workings of `next_token()`. Its structure is roughly:
- Boilerplate to handle stuff like newline, skipping whitespace and restarting lexing etc. I believe that this part can be written better but it's way too small and way too boring to refactor.
- If a digit is found, call `scan_int()` to scan an integer (this will put the integer value in `token.val`). We'll get back to `scan_int()` in a bit.
- Then follows a pretty obvious but powerful scheme which is "If you found a character that potentially starts a keyword and it actually starts a keyword then taraaah, here's your token. Save it and return. Otherwise, the token should be considered an id". For example, if you see `i`, that might start `int`. So, you check if the following 2 characters are `n` and `t`. The important thing is that you also have to check that the next character is a separator (like a space) because for example `inta` is not the keyword `int`.
- Ids are pretty easy but there's one important thing happening there and that's string interning (this happens by calling `str_intern_range()`). String interning is a very important topic for compilers so it has a section on its own.
- Last but not least, we have a bunch of operators and handling of one-line comments.

What has remained is `scan_int()` and `peek_token()`.

### scan_int()
Before trying to scan an int for the first time, I assumed that scanning an int is way easier than it actually is.
As with a lot of things in compilers, all the gymnastics are for handling and recovering from errors.
Its structure roughly is:
- Check the prefix to see if you have octal, hex or decimal.
- Compute the value. Here we need for each digit / char to get its numeric value using `digit_from_char`. Verify that it's in the limits of the base. Verify that it won't overflow the value (you might want to think why we don't write the more natural `if (val * 10 + digit > int_max)`. If it does, eat the remaining digits. Otherwise, update the value.

### peek_token()
As with every function, there are the questions "why", "what" and "how" as name them. Why do you need the function ? What the function has to do ? How does it do it ? I also like to follow this order when I'm coding. We should focus first on what the function needs to do before we start implementing it (or as Casey Muratori puts it, write the usage code first).
Here we'll go in the reverse order to stay on topic. That is, I'll explain the how. You can check the section on Parsing to learn about the why and the what.

`peek_token()` is very simple because of the fact that the _whole_ input is in one contiguous buffer. And we can move in this buffer arbitrarily back and forth. So, to implement `peek_token()` (which to the outsider is: return me the next token without advancing to it) is as easy as: Save the current globals, call next_token() to get the next token, save it, restore back what you saved, return the new token.

Finally, there are some handy functions for testing the lexer and the actual testing function.

### Acknowledgements
- The handling of keywords is similar to what [LCC](https://sites.google.com/site/lccretargetablecompiler/]) does (LCC is a very interesting project - I learned a lot from reading it).
- I picked the ideas for testing from [Bitwise](https://github.com/pervognsen/bitwise).
- The `charmap` idea is similar to what [DMD](https://github.com/dlang/dmd) does.

## Parsing
Let's start with a question: **Why did you hand-craft a parser instead of using a parser generator?**
This question has been answered many times from compiler writers and all compiler experts seem to agree on it (you can verify that either because they say so or because you see hand-crafted parsers in almost all
serious compilers). But it's not obvious to most people not actively involved in serious compilers.
I think that this is partly because most schools spend a lot of time in teaching about complicated parsing techniques and promoting parser generators (plus the Dragon Book spends way too much time on the topic).

Anyway, the fact remains that most serious compilers use hand-crafted recursive-descent parsers. That is because:
* They're easy to write.
* They're easy to understand.
* They're easy to debug / predict.
* They're easy to extend.
* They're fast.
* They help you print nice error messages.
* They generate exactly as much code as you need.

I'm not saying that parser generators don't have any of those but certainly they don't have all of those (they also have things that hand-crafted compilers don't have like speed of prototyping and just writing the grammar and be done with it).
Also, parser generators are fine if you know where to use them (as with any tool). For example, for hobby projects or generally something that does not necessarily need to be programmer-friendly, they're more than good.

### Abstract Syntax Tree (AST)
Now, to the actual inner workings of the parser. Τo explain the parser we have to talk about the AST. And we need to have the [grammar](http://cgi.di.uoa.gr/~thp06/project_files/minijava-new/minijava.html) at hand. It basically consists of expressions, statements, type specifiers and declarations. Also, it has a type for the main class and a type for the Goal, which is the whole AST. All those types are in `ast.h`

#### Expressions
The expressions are written in C-style. That is, they don't depend heavily on inheritance and they're using `union`s to couple together things that are never more than one needed at the same time (e.g. no expression needs the `id` _and_ the `int_lit` fields). So they can use the same space.
They're identified in the usual way in such situations - using an `enum`. We'll often `switch` this `enum` when manipulating expressions.
The only inheritance that there is is the `BinaryExpression`. Most binary expressions have 2 children, `e1` and `e2`, so they need the space of an `Expression` + for the second child. The space need for the second child is a pointer to an expression, which is usually 8 bytes (or less), so we could include that in every expression. I'm still not sure if it's worth it to have this inheritance.

Anyway, the last expression is the message send expression. This is a loner in that it needs 3 things to be saved. An expression, an identifier and an expression list. For the expression we use the `e1` and for the rest we give a pointer to outer data.

#### Statements
Normally all my AST nodes are similar to what I described for Expressions. But I decided to experiment with a more traditional inheritance hierarchy and virtual functions. This is a traditional compiler design but I'll
also describe a more unconventional approach based on the same ideas. Before continuing, I expect that you have some familiarity with the typical C++ / OOP bullshit like inheritance, (pure) virtual functions, abstract classes etc.
First and foremost, when you take that road, try to use single inheritance. Basically, if you need multiple inheritance _in any problem_, something has probably gone very badly in the design. It's a deeply flawed idea.
Then, ideally, all the nodes inherit directly or indirectly from a single node (here it is `Statement`) that may work as an abstract class. In C++, this is implemented using pure virtual functions. Now, when we do that,
everything we'd like to do with an object of that category (like a `Statement`) is done using virtual methods that are resolved in the runtime to the correct code (if that didn't make much of a sense, study virtual functions in C++). For example, if we want to print a statement, we have a `print()` method that is implemented differently in every sub-class.

During the parsing we don't want to do much with statements, for the most part we only construct them and maybe we want to get a printing of them or their name. It's the type-checking where all this becomes interesting. So for now, you probably want to ignore the `accept` methods.

**The unconventional approach**

I'd suggest that you read this part if you're comfortable with how Statement and Expression nodes work.
An alternative way to do what we did with statements is to have a single `Statement` node (much like an `Expression`) with a `kind` field again. Then, each virtual method that would be implemented in the sub-classes, is implemented by `switch`ing the `kind`. For example, what do now is:

```
struct Statement {
    virtual const char *name() const = 0;
};

struct BlockStatement : public Statement {
    Buf<Statement*> block;
    const char *name() const override {
        return "Block (Statement)";
    }
};

struct AssignmentStatement : public Statement {
    const char *id;
    Expression *rhs;
    const char *name() const override {
        return "AssignmentStatement";
    }
};
```
and if we have an `s` of type `Statement*`, `s->name()` will be resolved at runtime. The alternative is:

```
struct Statement {
	STMT kind;
	union {
		struct {
			const char *id;
			Expression *rhs;
		};
		Buf<Statement*> block;
	};
	const char *name() {
		switch (kind) {
		case STMT::BLOCK: return "Block (Statement");
		case STMT::ASGN: return "AssignmentStatement";
		default: assert(0);
		}
	}
};
```

I like this better because it's more flexible, clearer and faster. It's more flexible because it gives you the ability
to do what you do with virtual functions + the ability to check the kind of a statement. You can do `if (s->kind == ...)` with this but with the other not. It's more clear because it's obvious on how you execute the code that you actually execute while virtual functions are _somehow_ implemented (while using +8 bytes in every object for the virtual pointer) and _somehow_ the right function is called at runtime (note that even though most compilers implement virtual functions the same way, it's not standardized). It's faster more often than not because virtual functions incur a penalty because of the double-dereference (to understand this, you want to read about the implementation of virtual functions). It's even worse when you use the visitor pattern. This is where the whole devirtualization thing comes into play. The switch is definitely faster.

The drawback of this is that if the different kind of the _something_ (in our case, different kinds of statement)
don't require the same size (or almost the same size), then you might end up wasting too much space.

#### Type specifiers
Those are small and easy. If you're wondering what type specifiers are, they're the _syntactic_ structures to denote a type. For minijava, these are `boolean`, `int`, `int[]` or with an identifier.

#### Declarations
The declarations follow pretty much the same reasoning as `Statement`s. One important thing is that we use the same type to save a field, a variable and a param (a similar approach is take in the type-checking). The reason is simply that all 3 of them require the same things: a type specifier and an id.

#### Misc
- The `MainClass` and `Goal` I think are pretty self-explanatory.
- A field called `loc` is used for every node and it saves the location (which for this compiler it's the line) in the file.
- The top level nodes inherit from 2 classes called `ParsingTemporaryAllocation` and `ParsingPersistentAllocation`. These classes overwrite the `new` operator to allocate from our own allocator functions. There's a separate section for the allocator but the idea here is that the AST nodes are split into 2 separate life-time categories. The type-checking consists of 2 passes. Some nodes need to be kept for the second pass and some not. For example for every declaration, we save a type-checking-equivalent type and we use that from then on. And so, throw the AST node away. But for example the expressions are need all through the type-checking (and code generation).
- `Buf` is used a lot in the AST nodes. It is explained in a separate section.

### The actual parsing
The actual parsing happens mainly in the `parse.cpp` file along with some misc code in the `ast.cpp`.
The `parse.cpp` is a little big but I think it's understandable and it follows closely the [grammar](http://cgi.di.uoa.gr/~thp06/project_files/minijava-new/minijava.html). What is more, it uses a couple of basic ideas again and again.

#### match_token() and expect_token()
Those 2 functions are the basic building blocks of the parser. `match_token()` takes a token kind and checks if `token` (remember that this is the global that is `extern`ed from the lexer and it is where `next_token()` saves the next token) is of this kind. If yes, then it advances to the next token and returns true. If not, it returns false. A lot of times we want to check if the next token is the _x_ and if so, advance to the next token and go on. Otherwise, we want to take another guess. This is where `match_token()` is used.
The idea in `expect_token*()` functions is similar but these are used when the current token _must_ be of the kind that we pass them. Hence, if it's not, an error is issued. They also have a couple more features like tracking and of input, and giving an optional 2nd parameter that controls if we want to advance even if the token is not of the kind we want to check. Finally, specifically `expect_token_in_rule()` also issues a specific error. That we expected a specific token in a specific rule.

The rest of the parser is pretty much a normal recursive descent parser that follows closely the grammar, uses those building blocks and tries to do basic error recovery.

### Error recovery in parsing
I assume that a lot of people have not heard of the term (even if they have some experience in compilers).
A basic (maybe hobby) compiler stops on the first error it sees and maybe exits. This is not very useful in practice because if there are multiple errors you have for every one of them to: fix it, run the program to find the next error, repeat.
_I should note here that for a lot of basic compilers, this is not a problem. First of all, because this is a problem when you're compiling thousands (or millions) of lines of code. And no sane person would write a serious million lines of code to be compiled by a hobby compiler. Second, because these compilers compile simple languages, unless they're very badly engineered, they're fast even for thousands of lines of code (consider that the goal of this compiler is to compile about 1 million lines of code per 1.5s in the time of writing this). So, bottom-line, I did not solve this problem because it's an actual problem, but rather to experiment._

Back to the topic, some compilers try to solve by doing error recovery. That is, when they encounter an error, they try to somehow recover from it and continue their job. There's no right way to do this and it's highly dependent on the language. It is also something that it is like a trade and comes from experience.
Error recovery happens in parsing and in type-checking. For now, let's explain the parsing one. Note that out of many things, in parsing, we try to not only recover from errors sanely to continue _parsing_ but also to provide good nodes into the type-checking, so we can continue sanely there as well.

I decided to do some simple dichotomy in errors. Some errors are "fatal" and some not.

A "fatal" error (in quotes, because an actual fatal error means the program has to exit asap) happens when we encountered an error that does not let us continue following the same grammar and hence what we currently have as an "error" node. An example of this is in `parse_type_declaration()`. If we don't find an identifier after the keyword `class`, we expect that something has gone badly and we just `goto Lerror`. This `goto Lerror` along the parser goes to some part of the code that makes the current node invalid (or undefined). This is useful because for example we then completely skip these nodes in type-checking. 

A non-fatal error is e.g. a left brace. If we don't find one where we should, we e.g. just assume that it's there and continue our job.

#### skip_tokens()
This is the last function we have not described. This just skips tokens until it finds one of a specific kind. I picked the idea from  [LCC](https://sites.google.com/site/lccretargetablecompiler/]). They use a more powerful version of it and I'd like at some point to come back and revise this if I can find good uses of more powerful versions.

### The dirty bit of the parser
I think that most of the parsing code is good and clean. Except for expressions. Specifically, `parse_expr()`. It feels very weird and not robust. I will try to improve it at some point in the future but I'm just giving a warning here.

### The one place where we need to peek a token
This grammar is mostly [LL](https://en.wikipedia.org/wiki/LL_grammar)(1). In short, this means that we can decide in which we're rule we should go to only by looking the next token. Where this breaks in this language (again, I'm pointing the [grammar](http://cgi.di.uoa.gr/~thp06/project_files/minijava-new/minijava.html) used) is when we have to decide where the local declarations end for a method and the statements start. Note in the grammar that first _all_ the local declarations are written, then _all_ the statements. They can't be intermixed.
The problem is that an identifier can start a local declaration (like `A a;`) or it can start a statement (like `A = ...` or `A[...] = ...`). So, while we're parsing local declarations, if we see an identifier, we don't if it's going to start a statement or another declaration.

#### Context-free grammars
One solution to this is to say "Well, it starts a declaration if the identifier corresponds to a type and it starts a statement if it corresponds to local variable. Otherwise it's invalid". The problem with this solution is that it now makes the language _not_ context-free. You can find a lot of formal info about what this means specifically, but in short, it means a token (kind) does not suffice by itself to help us decide which rule to follow. We have to know the _meaning_ of the token (i.e. semantic info about it). But meaning is attached in the semantic analysis (or type-checking) phase, which comes later. This is **no good!**. It's not that we can't do it, but it results in not cleanly written compilers, separated cleanly into phases.

The way to solve the problem is of course to peek the next token. If the next token is an assignment or a left bracket, we assume it tries to start a statement. Otherwise, we assume it tries to start a local declaration.

### Last notes and Acknowledgements

Normally, in MiniJava compilers, the lexing and parsing are very boring and they're even usually automatically generated. I hope that all the error-recovery and hand-writing was somewhat more interesting to you (as it was very much to me).

- Basic versions of `match_token()` and `expect_token()` come from [Bitwise](https://github.com/pervognsen/bitwise).

## Type-checking / Semantic Analysis
Ok, parsing set the stage for the fun parts, but now it becomes serious. The type-checking is separated into 2 passes.

### Why do we need 2 passes ?

We pretty much need 2 passes in any language that has order-independent declarations. That is, when it is acceptable to use something before it's declared. For example, in Java:
```
class A {
  B b;  // <- no error although B is declared later
};
class B { }
```
You don't have that in C / C++. If you've ever heard about cyclic dependencies in C and C++ because of the header files, that's one reason this happens. If you've also heard about 1-pass compilers in C / C++, this is one reason it can be done.
_Note: What we have till now is 3 passes. The parsing is one pass (as it happens in parallel with lexing) and 2 passes for type-checking._

### The visitor pattern
I won't describe the visitor pattern, but I'll say that I used this too in order to experiment. Honestly, I don't see the visitor pattern as amazingly useful and it puzzles me that some people think that compilers can't be written without it. On the other hand, it needs less work to have a clean result. I want to stress out 2 things:
- Thanks to [DMD](http://github.com/dlang/dmd) and [LDC](https://github.com/ldc-developers/ldc) developers. Through studying the code of these 2 compilers I learned how to write pragmatic and useful visitors.
- Thanks to [Bitwise](https://github.com/pervognsen/bitwise) for teaching me how to write a type-checker without a visitor.

As the 2nd note above pointed, semantic analysis can be done without a visitor and let's the way.
Of course the way is: use a the couple of `kind` field and `switch`.
**-- The visitor version --**
```
class Visitor {
	void visit(Foo *foo) {
		// Do what you want to with a Foo
	}
	void visit(Bar *bar) {
		// Do what you want to with a Bar
	}
};
class Foo {
	void accept(Visitor *v) { v->visit(this); }
};
class Bar {
	void accept(Visitor *v) { v->visit(this); }
}
```

**-- The kind and switch version--**
```
enum class KIND {
	FOO,
	BAR,
};
struct Thing {
	KIND kind;
	union {
		// The whatever fields union'ed.
	};
};

void process_a_thing(Thing *thing) {
	switch (thing->kind) {
	case THING::FOO: {
		// Do what you want to with a Foo
	} break;
	case THING::BAR: {
		// Do what you want to with a Bar
	} break;
	default: assert(0);
}
```

### Structures used
Before going to pass 1, I'll briefly explain the structs used in type-checking. Basically, there are the 2 visitors for pass 1 and 2 and the structs `Type`, `Local` and `Method` which are the respective types of the AST nodes. In type-checking, we throw the AST counterparts and replace them with those that hold only the info for which we care about, which is the semantic info (not the structural info). I'd suggest that you don't start with understanding these structs wholly, but rather incrementally, as you understand the passes.

#### A note about Type and IdType
The `Type` struct is used to hold types. You can see there is also a child struct, `IdType`. The reason that we have the latter is because identifier types (the ones that are used to represent class declarations) need quite more info, like the identifier and hash tables for methods and fields. So, we have a different struct to not waste space when we only need a `Type`. In hindsight, that was a very bad decision. We'll only going to have 3 types that are _not_ `IdType`s, the 3 primitives. So, no actual waste of space while the separation of those makes the handling of types way more difficult. It will be changed in a future version.

### Pass 1
In the first pass we try to gather as much info as we can in order to use it the second pass, while also trying to catch errors early. We concern ourselves only with the declarations. Why only with declarations ?
This is something that might be obvious to some while very obscure to others. I'll give an over-simplified answer for this guide. Declarations declare types that we then use. And we use them to do things. And we do things with _statements and expressions_. We can't possibly _do_ anything with something we don't fully know. And since we have order-independent declarations, it's possible that a statement or expression uses something not yet declared (so, something we don't know about yet).

Essentially, we're dealing with 3 kinds of declarations. Type declarations, method declarations and local declarations, which correspond to the respective AST types (remember that a local declaration is used for a local variable, a parameter and a field).


#### The type table
The basic data structure of the whole type-checking is the type table and this is to be filled during the pass 1. To start off, it "interns" the 3 primitive types (`int`, `int[]` and `boolean`) so they're saved only once. When we want to say that something has `int` type, we assign it `int_type`. Then, to check if a thing is of type `int`, we only have to pointer comparison because there's only one type describing `int` around.

The same basic idea follows to heart of the type table, which is the hash table of `IdType`s. `IdType`s denote the user-defined types. The idea here is to have every user-defined type saved only once and then just do pointer comparison when we want to check for type equality. The process goes as follows:
- **Step 1**: If we see an id type-specifier (remember, the parsing description of a type), we query the table with this id to check if it's already there. So, if we see `public A foo(...) {`, the `A` is an id type-specifier with id of course being A. We query the table and let's say it's not there (so, it's the first time we encounter A). Then, we insert an entry. This entry is a pointer. Now, we point to the space allocated for type `A` and now the return type of method `foo` points to this space. We can see a type specifier in: _a field declaration_, _a method return type-specifier_, _a parameter declaration_, _a local declaration_ or after the `extends` keyword. These are places that **use** `A`.
- **Step 2**: At some point (hopefully) we will see a class declaration for `A`. Then, we query the table to see if it's there. In the running example, it's there. So, _we get a pointer to this type_. And we start filling its details according to the class declaration. The magic thing is that foo's return type and this pointer we just got, point to _the exact same space_. This is important because now foo has access to the just filled `A` for free. We don't need to go back to foo and say "Oh hey, I just saw, here's its details". No, the details are filled to the space that both point to.

Note that who does the allocation can reversed with the same effect. i.e. the class declaration can come first, the the use of `A`.

Now, the other important thing is that let's assume in step 1, `A` was there. In that case, we get back a pointer that say points to address 0x200. Then, we see a local declaration in some other function is other unrelated class: `A a`. Following the same procedure (again, step 1), we will query the table with `A`, and we will get back _again_ the address 0x200. That's super important because now a type is essentially identified by address. Meaning, every type corresponds to a specific address. And everything that has that type points to this address. So, if I then do: `a = foo(...)`, to check if what `foo()` returns can be assigned to `a`, I have to check if they're types are equal. Since because of all the above if that's true, the 2 types will point to the same address, I have to just do a pointer comparison (it's a little bit more complicated because of inheritance and stuff, but you get the point).

This pretty much what pass 1 is all about. It does some other stuff too like checking for redefinition of fields, parameters, locals and methods. That is, if we have a declaration with the same id as another declaration. For example:
```
A a;
B b;
int a;  // <- redefinition
```
These are done as you might expect: with a hash table. The usual: insert it if it's not there, otherwise if it is, we have redefinition. Note here that there's no overloading, so this:
```
public int foo(int a) { }
public int foo(int[] a) { }
```
is invalid.

#### Important details
You might want to skip that for now if what I described above is not yet very clear. Here are some important details that I glossed over. Most of them have to do with inheritance.
- A field of a child class can have the same name with a field of the parent class and this is called shadowing. When we query the object, we pick the most immediate field in the inheritance tree.
- For methods, checking for redefinition is not enough. We also have to check if it overrides a method of the parent class. That can't happen in pass 1 because we might have not processed the parent yet.
- You might think that pass 1 can be coupled with parsing because hey.. you just insert things in the type table as you see them. The problem is that this is potentially slower because of the allocations and it alters the implementation of the type table. One important detail of the hash table (and all the hash tables used in type-checking) is that we know _exactly_ how many elements we need to store. When we do pass 1 after parsing, we know that we have `x` number of type declarations (although, there's a caveat here that I reference below). It's not flexible and hence we can make a simple yet very efficient linear-probing hash table (more on the hash table in the section of data structures).
- Continuing from the above, one question is what do you do if the used unique types are more than the type declarations. Note that in almost all cases, where the programs are correct (or almost correct), the types used are almost as much as as the type declarations. However, if they're more, it just means we simply don't have space in the table to store them. I use a `could_not_be_inserted` buffer to store those that I then iterate over and print messages. This has to be made better because it can print false positives. There's a comment describing this.

### Pass 2

Pass 2 is more straightforward than you may think and I believe that for the most part, the code is pretty self-explanatory. I will describe the subtle parts which again, mostly have to do with inheritance.
- **Cyclic inheritance**: There is the possibility to have cyclic inheritance, which is what you imagine. An example:
```
class A extends B { }
class B extends A { }
```
This is not optional to test because if cyclic inheritance exists, there's high chance that the program will get to infinite loop.
Every `IdType`, if it inherits, has its `parent` field point to the parent. Checking for cyclic inheritance, say `A`, is as simple as first keeping a reference to `A`. Then, going through its parents until we find `NULL`. If along the way, a parent is equal to `A`, we have cyclic inheritance. The tricky part is that we have to account for cyclic inheritance in _every_ place we iterate through the parents. That's bad code except for duplicate code and I have a note to do something smarter. It seems like a good place for a C++ iterator that hides these checks.
- **Check if a method overrides**: The rules as follows:
-- A method can conflict with a method of the parent class.
-- If the methods have the same return type (but possibly different formal
    parameters), it's valid overriding (we're calling the most immediate one in the inheritance tree).
-- Otherwise, if the methods have different return types but the types
of the parameters don't match _exactly_, then it's valid overriding (consider that if they do, we have no way to disambiguate which one we mean).
-- Otherwise, it's an error.
One tricky part here is that a method can override with a method with _any_ of its parents in the inheritance tree.

- **Calling a method**: When we call a method, because of overriding and inheritance, we have to possibly check the whole inheritance tree to see if a matching method is found.
- **Assignment**: We can generalize the concept of assignment into any case where we put a value of one place to another. For example:
```
A a = a2;       // assignment
a.foo(b);        // assignment, we assign the argument b to the formal parameter of foo()
a = a.foo();    // assignment - we assign the return value of foo() to a
a.foo(a.foo());  // assignment - we assign the return value of foo() to the formal parameter of foo()
```
We generalize it because of common rules in all these cases. Specifically, the types of the "left-hand-size" and the "right-hand-size" have to be compatible. If we didn't have inheritance, "compatible" would mean "equal". But because of inheritance, we can assign `B` to `A` if `B` is a child of `A`.

## Codegen
To be continued...
