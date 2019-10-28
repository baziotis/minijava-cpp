#ifndef ALLOC_H
#define ALLOC_H

#include <stdio.h>    // size_t, ssize_t

enum class MEM { STR_INTERN, PARSE_TEMP, PARSE_PERSIST_TYPECHECK_PASS2,
                TYPECHECK, FUNC, __LEN };

void *allocate(size_t n, MEM enum_ar);
void *allocate_zero(size_t n, MEM ar);
void deallocate(MEM enum_ar);

#endif
