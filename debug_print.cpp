#include <stdio.h>
#include <stdarg.h>

#include "debug_print.h"
#include "error.h"

static int __call_depth = 0;
static int indent_char;

LogScope::LogScope() {
    ++__call_depth;
}

LogScope::~LogScope() {
    --__call_depth;
}

void print_indentation() {
    if (__call_depth > 0) {
        for (int i = 0; i != __call_depth - 1; ++i)
            printf("%c ", indent_char);
    }
}
    
void debug_print(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    print_indentation();
    vprintf(fmt, args);
    va_end(args);
}

void debug_line(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    debug_print(fmt, args);
    printf("\n");
    va_end(args);
}

void set_indent_char(int c) {
    indent_char = c;
}
