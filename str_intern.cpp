#include <vector>

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "buf.h"
#include "str_intern.h"

/*
********************************** STRINGS ****************************************
    
    String interning utilities.

***********************************************************************************
*/

// TODO:
// 1) Use linear-probing hash table.
// 2) Separate strings in different hash tables according to length.
//    For example, strings of len <= 4, len <= 16, len <= 32 and finally arbitrary length.
//    In the first 3 categories, the string contents should be saved inside the table
//    (i.e. not with a pointer pointing to the contents).
//    In the category len <= 4, we can omit to save the length.
//    The contents of strings of arbitrary length should use a custom allocator. In the extreme case,
//    this allocator has to be sophisticated, in that, nearby buckets of the table
//    should use nearby memory locations (for the string contents).

/*
struct InternedStr {
	size_t len;
	const char *str;
};
*/

Buf<InternedStr> strings;

/// Intern a string starting from `str` with length `len`.
const char *str_intern_len(const char *str, size_t len) {
    assert(str);
    
    for (const InternedStr &i : strings) {
        // Hope for the compiler to do sophisticated optimization
        // to handle small strings fast.
        //printf("%zd %zd %.*s %s\n", len, i.len, (int)len, str, i.str);
        if (i.len == len && !memcmp(str, i.str, len)) {
            // Already saved, return the pointer.
            return i.str;
        }
    }
    char *str_copy = (char*) malloc(len + 1);
    memcpy(str_copy, str, len);
    str_copy[len] = 0;
    strings.push(InternedStr{len, str_copy});
    return strings.back().str;
}

/// Intern a NUL-terminated string.
const char *str_intern(const char *str) {
    // TODO-FUN(stefanos): Do a SIMD implementation of `strlen`.
    return str_intern_len(str, strlen(str));
}

/// Intern a string range
const char *str_intern_range(const char *start, const char *end) {
    return str_intern_len(start, end - start);
}
