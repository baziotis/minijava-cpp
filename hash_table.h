#ifndef HASH_TABLE_H
#define HASH_TABLE_H

// Parallel buffers of ids (const char *) and value (e.g. Method)
// for faster searching in the ids. The ids are interned
// so simple pointer comparison works.

template<typename T>
struct HashTable {
    const char **ids;
    // Important: You want to use this hash table with small
    // data. Like pointers for example, so that space
    // is not wasted.
    T *data;
};

#endif
