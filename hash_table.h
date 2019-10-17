#ifndef HASH_TABLE_H
#define HASH_TABLE_H

#include <type_traits>  // std::is_pointer

#include "alloc.h"
#include "buf.h"

// Parallel buffers of ids (const char *) and value (e.g. Method)
// for faster searching in the ids. The ids are interned
// so simple pointer comparison works.

// TODO: T is Q*. Impose that Q should have an `id` field so that we
// pass one thing on insert()?

// Usage notes:
// 1) A pointer should always be passed as T.
// 2) It's the user's responsibility to not insert duplicate `id`.

template<typename T>
struct HashTable {
    static_assert(std::is_pointer<T>::value, "T must be pointer");

    Buf<const char *> ids;
    Buf<T> data;

    HashTable() { }

    HashTable(size_t n) {
        this->reserve(n);
    }

    void reserve(size_t n) {
        ids.reserve(n);
        data.reserve(n);
    }

    void insert(const char *id, T v) {
        assert(this->find(id) == NULL);
        ids.push(id);
        data.push(v);
    }

    T find(const char *id) {
        size_t i = 0;
        for (const char *ident : ids) {
            if (ident == id) {
                return data[i];
            }
            ++i;
        }
        // Can do that because we have verified that T is a pointer.
        return nullptr;
    }
};

#endif
