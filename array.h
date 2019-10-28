#ifndef ARRAY_H
#define ARRAY_H

#include <assert.h>
#include <limits>

#include "alloc.h"

#define MAX(a, b) ((a > b) ? a : b)

// A constant capacity buffer without C++ magic.

template<typename T, MEM mem>
struct Array {
    // Members
    size_t cap, len;
    T *data;
    
    Array() {
        cap = len = 0;
        data = nullptr;
    }
    
    Array(size_t n) : Array() {
        reserve(n);
    }
  
    // No destructor, no copy and rvalue constructors.
    // It works like a C struct.

public:
    void push(T v) {
        constexpr size_t size_t_max = std::numeric_limits<size_t>::max();
        assert(len < size_t_max);
        assert(len < cap);
        size_t new_len = len + 1;
        data[len] = v;
        len = new_len;
    }
    
    void reserve(size_t n) {
        assert(len == 0 && cap == 0);
        if (!n) return;
        data = (T *) allocate(n * sizeof(T), mem);
        assert(data);
        len = 0;
        cap = n;
    }
    
    T& operator[](size_t i) { assert(i < len); return data[i]; }
    const T& operator[](size_t i) const { assert(i < len); return data[i]; }
    
    inline T* begin() { return this->data; }
    inline const T* begin() const { return this->data; }
    
    inline T* end() { return &this->data[len]; }
    inline const T* end() const { return &this->data[len]; }
};

#endif
