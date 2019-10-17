#ifndef ERROR_H
#define ERROR_H

#include "tokens.h"
#include "common.h"
#include "ast.h"

static void log(location_t loc);
static void log(TOK kind);
static void log(EXPR kind);
static void log(int i);
static void log(const char *s);
template <typename T, typename... Args> 
static void log(T t, Args... args);

template <typename... Args> 
static void internal_compiler_error(Args... args);

template <typename... Args> 
static void warning(Args... args);

template <typename... Args> 
static void error(Args... args);

template <typename... Args> 
static void syntax_error_no_ln(Args... args);

template <typename... Args> 
static void syntax_error(Args... args);

template <typename... Args> 
static void fatal_error(Args... args);

#include "error.cpp"

#endif
