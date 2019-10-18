#include <assert.h>
#include <stdlib.h>    // malloc()
#include <stdint.h>    // uint8_t
#include <string.h>    // memset()

#include "alloc.h"     // MEM

/********************************** ALLOC *****************************************
    
    Memory allocation and de-allocation utilities.

***********************************************************************************

    - Each memory arena is a list of contiguous blocks. Its pointer points
      to the current one (which is the last) in use.
    - Freeing an arena puts all its blocks in a list of free blocks.
*/

static constexpr size_t kilobytes(ssize_t n) {
    return n*1024;
}

static constexpr size_t roundup(size_t x, size_t n) {
    return ((x)+((n)-1)) & (~((n)-1));
}

constexpr int NARENAS = (int)MEM::__LEN;
constexpr ssize_t ALIGN_SIZE = sizeof(void*);
constexpr ssize_t BLOCK_TAIL = kilobytes(10);

// NOTE(stefanos): A block is naturally aligned because it only contains pointers.
// This is true for 32-bit systems as well since e.g. a 64-bit value is going to be
// emulated.
struct block_t {
    block_t *next;
    uint8_t *avail;
    uint8_t *limit; 
};

// TODO: Insert testing code that we match NARENAS
// Holder of the first block which is empty. Used only to specify empty
// arenas.
block_t first[NARENAS]= { { NULL }, { NULL }, { NULL }, { NULL } };
// The actual arenas
block_t *arena[NARENAS] = { &first[0], &first[1], &first[2], &first[3] };

// List of free blocks. When an arena is free'd
block_t *freeblocks;

/*
    allocate: Give pointer to free contiguous memory of at least size `sizeof (T)`.
-------------------------------------------------------------------------
    This internally means do one of the 3:
    1) If there's available space on the current block in use (the last
       block put in the arena), return a pointer and update `avail`.
    2) If not, then search the `freeblocks` list for a big enough block.
    3) If none is found, then allocate a new block and put it in the tail
       of the arena. Update its pointer to point to this block.
*/

/// Get limit of a current block of an arena. This is used
/// to check if we have changed block.
// Maybe get_arena_info() and also give the avail?
static uint8_t *get_block_limit(MEM ar) {
    return arena[(int)ar]->limit;
}

/// Raw size version
void *allocate(size_t n, MEM enum_ar) {
    int ar = (int) enum_ar;
    block_t *ap = arena[ar];
    assert(ap);
    
    n = roundup(n, ALIGN_SIZE);
    if (n > (ap->limit - ap->avail)) {
        ap->next = freeblocks;
        if (ap->next != NULL) {
            do {
                freeblocks = freeblocks->next;
                ap = ap->next;
            } while(n > (ap->limit - ap->avail));
        } else {  // Need to allocate
            size_t sz = sizeof(block_t) + n + roundup(BLOCK_TAIL, ALIGN_SIZE);
            // Set `next` to the new block first so that if this is the first block
            // in the arena `ar`, then first[ar].next will be set to this new block.
            ap->next = (block_t *)malloc(sz);
            ap = ap->next;
            assert(ap && "Insufficient memory");
            ap->limit = (uint8_t*)ap + sz;
        }
        ap->avail = (uint8_t*)(ap + 1);
        ap->next = NULL;
        arena[ar] = ap;
    }
    ap->avail += n;
    return (void*)(ap->avail - n);
}

void *allocate_zero(size_t n, MEM ar) {
    void *mem = allocate(n, ar);
    // Zero it
    memset(mem, 0, n);
    return mem;
}

/*
    deallocate: Deallocate a whole arena.
-------------------------------------------------------------------------
    This internally means to take its list of blocks (which start at its
    first->next) and put in the freeblocks list.
*/
// Internally, that means 
void deallocate(MEM enum_ar) {
    int ar = (int) enum_ar;
    // Connect the arena's last block with the start of free blocks list
    arena[ar]->next = freeblocks;
    // Now point the start of the free blocks list to the first block of the arena
    // (which is the start of the chain connecting arena's block and the free blocks)
    freeblocks = first[ar].next;
    // Set the arena's first block to nothing again.
    first[ar].next = NULL;
    // Point again the arena to the first empty element
    arena[ar] = &first[ar];
}


/* Testing
 */
/*
static bool pointer_in_range(void *p, void *start, void *end) {
    return (p >= start && p <= end);
}

void test_alloc() {


    void *a = allocate(123, MEM::PARSE);
    void *b = allocate(10, MEM::PARSE);
    // Verify that we're on the same block and simultaneously
    // alignment.
    // If alignment and allocation worked correctly, we should meet `b`
    // from the start of the block + 123 rounded up to ALIGN_SIZE;
    uint8_t* temp = (uint8_t*)a + roundup(123, ALIGN_SIZE);
    assert(temp == b);

    block_t *ap = (block_t*) ((uint8_t*)a - sizeof(block_t));
    assert(((ap->avail - roundup(10, ALIGN_SIZE)) - (uint8_t*)a) == roundup(123, ALIGN_SIZE));

    // This should make it allocate different block
    void *c = allocate(BLOCK_TAIL, MEM::PARSE);
    assert(!pointer_in_range(c, ap, (uint8_t*)ap +
            sizeof(block_t) + roundup(123, ALIGN_SIZE) + BLOCK_TAIL));

    // Deallocate the arena
    deallocate(MEM::PARSE);
    // Verify that the freeblocks list contains exactly 2 blocks.
    assert(freeblocks != NULL);
    int count = 0;
    block_t *runner = freeblocks;
    while (runner != NULL) {
        ++count;
        runner = runner->next;
    }
    assert(count == 2);

    // Verify that I can re-allocate in 0.
    void *d = allocate(40, MEM::PARSE);
    assert(d != NULL);

    // Use a new arena but verify that we will take the second block
    // of `freeblocks`. This is the one we got with `c`.
    void *e = allocate(BLOCK_TAIL, MEM::TYPECHECK);
    assert(e == c);
}
*/
