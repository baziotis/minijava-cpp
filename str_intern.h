#ifndef STR_INTERN_H
#define STR_INTERN_H

struct InternedStr {
	size_t len;
	const char *str;
};

/// Intern a NUL-terminated string.
const char *str_intern(const char *str);

/// Intern a string range
const char *str_intern_range(const char *start, const char *end);

/// Intern a string starting from `str` with length `len`.
const char *str_intern_len(const char *str, size_t len);

#endif
