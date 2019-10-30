#include <stdio.h>  // printf()
#include <stdlib.h> // exit()

#include "common.h"
#include "tokens.h"
#include "ast.h"
#include "typecheck.h"

extern config_t config;
extern location_t loc;
extern const char *filename;

// IMPORTANT: The decisions below may _significantly_ slow down compilation.
// Essentialy, we're left off with bad C++... There are 2 choices. Either this
// or printf-like functions, which are horrible as well.

static void log(int i) {
    printf("%d", i);
}

static void log(size_t s) {
    printf("%zd", s);
}

static void log(char c) {
    printf("%c", c);
}

static void log(const char *s) {
    printf("%s", s);
}

static void log(location_t loc) {
    printf("%s: %d: ", filename, loc.ln);
}

static void log(TOK kind) {
    printf("%s", token_name(kind));
}

static void log(token_t tok) {
    printf("%s", token_name(tok.kind));
    if (tok.kind == TOK::INTLIT) {
        printf(" %d", tok.val);
    } else if (tok.kind == TOK::ID) {
        printf(" %s", tok.id);
    }
}

static void log(EXPR kind) {
    printf("%s", expr_name(kind));
}

static void red_on() {
    if (config.ansi_style) {
        printf("\x1b[31m");
    }
}

static void yellow_on() {
    if (config.ansi_style) {
        printf("\x1b[93m");
    }
}

static void yellow_off() {
    if (config.ansi_style) {
        printf("\x1b[0m");
    }
}

static void red_off() {
    if (config.ansi_style) {
        printf("\x1b[0m");
    }
}

static void bold_on() {
    if (config.ansi_style) {
        printf("\x1b[1m");
    }
}

static void bold_off() {
    if (config.ansi_style) {
        printf("\x1b[0m");
    }
}

// TODO: Find a way to iteratve over instead
// of recurse.
template <typename T, typename... Args> 
static void log(T t, Args... args) {
    log(t);
    log(args...);
}

template <typename T, typename... Args> 
static void debug_log(T t, Args... args) {
    if (config.log) {
        log(t);
        log(args...);
    }
}

template <typename... Args> 
static void internal_compiler_error(Args... args)  {
    bold_on();
    printf("Internal Compiler Error: ");
    bold_off();
    log(args...);
    printf("\n");
}

template <typename... Args> 
static void warning(Args... args) {
    log(loc);
    bold_on();
    printf(" Warning: ");
    bold_off();
    log(args...);
    printf("\n");
}

template <typename... Args> 
static void syntax_error_no_ln(Args... args) {
    log(loc);
    red_on();
    bold_on();
    log(" Syntax Error: ");
    bold_off();
    red_off();
    log(args...);
}

template <typename... Args> 
static void syntax_error(Args... args) {
    syntax_error_no_ln(args...);
    printf("\n");
}

template <typename... Args> 
static void fatal_error(Args... args) {
	printf("Fatal error: ");
	log(args...);
	printf("\n");
    exit(1);
}
