#ifndef HASH_TABLE_H
#define HASH_TABLE_H

#include <stdio.h>

#include <assert.h>
#include <stdint.h>
#include <type_traits>
#include "alloc.h"

// Parallel buffers of ids (const char *) and value (e.g. Method)
// for faster searching in the ids. The ids are interned
// so simple pointer comparison works.

// TODO: T is Q*. Impose that Q should have an `id` field so that we
// pass one thing on insert()?

// Usage notes:
// 1) A pointer should always be passed as T.
// 2) It's the user's responsibility to not insert duplicate `id`.

template <typename T, MEM mem>
struct __HashTable
{
    static_assert(std::is_pointer_v<T> && !std::is_same_v<T, void*>,
        "T must be a pointer and cannot be void *");

    __HashTable() { }

    explicit __HashTable(size_t nelements) {
        this->reserve(nelements);
    }

    // TODO: Uncomment those
    // Disallow Copy Constructor
    //__HashTable(const __HashTable<T, mem> &rhs) = delete;
    //__HashTable &operator=(const __HashTable<T, mem> &rhs) = delete;
    // Disallow rvalue reference copying
    //__HashTable(__HashTable<T, mem> &&rhs) = delete;
    //__HashTable &operator=(__HashTable<T, mem> &&rhs) = delete;

    inline void reserve(size_t nelements) {
        nbuckets = nelements / 0.7;
        ids = (const char **) allocate_zero(nbuckets * sizeof(const char *), mem);
        data = (T *) allocate_zero(nbuckets * sizeof(T), mem);
        for (size_t i = 0; i != nbuckets; ++i) {
            ids[i] = nullptr;
        }
    }

    inline size_t hash(const char *key) {
        // Note: We have asserted that the key is not NULL,
        // by definition, which leads to a fast and good
        // multiplicative hasing.

        // Prime factor
        uint32_t factor = 100001029;
        // Non-zero key gets hashed
        size_t h = (uint32_t) ((uintptr_t)key * factor);
        h = (h * (uint64_t) this->nbuckets) >> 32;
        return h;
    }

public:

    inline void insert(const char *key, T value) {
        assert(key);
        size_t bucket = this->hash(key);
        size_t i = bucket;
        bool wrapped = false;
        while (!wrapped) {
            if (this->ids[i] == nullptr) {
                this->ids[i] = key;
                this->data[i] = value;
                return;
            } else if (this->ids[i] == key) {
                printf("\tDUPLICATE: %s\n", key);
                // Duplicates are not allowed
                assert(false);
            }
            ++i;
            if (i == this->nbuckets) i = 0;
            wrapped = (i == bucket); 
        }

        assert(false);
    }

    inline T find(const char *key) {
        assert(key);
        size_t bucket = this->hash(key);
        size_t i = bucket;
        bool wrapped = false;
        while (!wrapped) {
            if (this->ids[i] == key) { 
                return this->data[i];
            } else if (this->ids[i] == nullptr) {
                return nullptr;
            }
            ++i;
            if (i == this->nbuckets) i = 0;
            wrapped = (i == bucket);
        }
        return nullptr;
    }

private:
    size_t nbuckets;
    const char **ids;
    T *data;
};

#endif
