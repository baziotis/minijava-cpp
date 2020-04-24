#ifndef ALLOC_H
#define ALLOC_H

#include <stdio.h>    // size_t, ssize_t

enum class MEM {
  STR_INTERN,  // Used to allocate space for string interning hash table.
  PARSE_TEMP,  // Used to allocate memory for parsing objects that are useless
               // (and gets de-allocated) after pass 1 of type-checking.
  PARSE_PERSIST_TYPECHECK_PASS2,  // Used to allocate memory for parsing objects
                                  // that need to persist through type-checking
                                  // and codegen.
  TYPECHECK,   // Used to allocate memory for type-checking / codegen.
  FUNC,        // Used for memory that is only used per function and then
               // gets de-allocated.
               // TODO: Locals and stuff could be saved there and not persist.
  VTABLE,      // Used only during the generation of vtables and then gets de-allocated.
  __LEN
};

void *allocate(size_t n, MEM enum_ar);
void *allocate_zero(size_t n, MEM ar);
void deallocate(MEM enum_ar);

#endif
