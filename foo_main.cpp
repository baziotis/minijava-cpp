#include <iostream>
#include "alloc.h"
#include "hash_table.h"

template <typename T> using THashTable = HashTable<T, MEM::TYPECHECK>;

int main() {
    THashTable<double *> foo{10}; // This is ok.
    THashTable<void *> foo_void{20}; // This should fail.
    THashTable<int> foo_int{30}; // This should also fail.
    return 0;
}