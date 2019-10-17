#ifndef HASH_TABLE_H
#define HASH_TABLE_H

#include <assert.h>
#include <cstring>
#include <functional>
#include <type_traits>
#include "alloc.h"

template <typename T, MEM mem, typename H = std::hash<char *>>
struct HashTable
{
    static_assert(std::is_pointer_v<T> && !std::is_same_v<T, void*>,
        "T must be a pointer and cannot be void *");

    HashTable() = delete;

    explicit HashTable(size_t buckets)
        : size{buckets}
        , ids{static_cast<const char **>(allocate_zero(buckets, mem))}
        , data{static_cast<T *>(allocate_zero(buckets, mem))}
    {
        for (size_t i = 0U; i != size; ++i) {
            ids[i] = nullptr;
            data[i] = nullptr;
        }
    }

    HashTable(const HashTable<T, mem, H> &rhs) = delete;
    HashTable &operator=(const HashTable<T, mem, H> &rhs) = delete;
    HashTable(HashTable<T, mem, H> &&rhs) = delete;
    HashTable &operator=(HashTable<T, mem, H> &&rhs) = delete;

public:

    inline void insert(const char *key, T value) {
        size_t bucket = hasher(const_cast<char *>(key)) % size;
        size_t i = bucket;
        bool wrapped = false;
        while (!wrapped) {
            if (ids[i] == nullptr) {
                ids[i] = key;
                data[i] = value;
                return;
            } else if (ids[i] == key) {
                assert(false);
            }
            i = (i + 1U) % size;
            wrapped = (i == bucket); 
        }

        assert(false);
    }

    inline T find(const char *key) {
        size_t bucket = hasher(key) % size;
        size_t i = bucket;
        bool wrapped = false;
        while (!wrapped) {
            if (ids[i] == key) { 
                return data[i];
            } else if (ids[i] == nullptr) {
                return nullptr;
            }

            i = (i + 1U) % size;
            wrapped = (i == bucket);
        }
        return nullptr;
    }

    inline bool contains_key(const char *key) {
        size_t bucket = hasher(key) % size;
        size_t i = bucket;
        bool wrapped = false;
        while (!wrapped) {
            if (ids[i] == key) {
                return true;
            } else if (ids[i] == nullptr) {
                return false;
            }
            i = (i + 1U) % size;
            wrapped = (i == bucket);
        }
        return false;
    }

private:
    const char **ids;
    size_t size;
    T *data;
    H hasher;
};

#endif
