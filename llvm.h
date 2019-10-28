#include "alloc.h"

enum class LLVALUE {
    UNDEFINED,
    CONST,
    REG,
};

struct llvalue_t {
    LLVALUE kind;
    union {
        long reg;
        int val;
    };

    llvalue_t() { }


    llvalue_t(bool _val) {
        kind = LLVALUE::CONST;
        val = (int) _val;
    }

    llvalue_t(LLVALUE _kind, int _val) {
        assert(_kind == LLVALUE::CONST);
        kind = _kind;
        val = _val;
    }
    llvalue_t(LLVALUE _kind, long _reg) {
        assert(_kind == LLVALUE::REG);
        kind = _kind;
        reg = _reg;
    }
};

struct llvm_label_t {
    const char *lbl;
    bool generated;

    llvm_label_t() : lbl(NULL), generated(false) { }

    llvm_label_t(const char *_lbl) {
        // Assume that the label is string literal
        lbl = _lbl;
        generated = false;
    }

    void construct(const char *_lbl, long num = -1) {
        assert(!generated);
        assert(_lbl);
        assert(lbl == NULL);
        if (num == -1) {
            long num = gen_lbl();
        }

        // Labels are allocated in MEM::FUNC arena.
        // This arena is freed when we finish a method.

        size_t len = strlen(_lbl);
        // Allocate space that will surely be enough.
        size_t alloc_size = len + 20;
        char *p = (char *) allocate(alloc_size, MEM::FUNC);
        // sprintf might be slow.
        sprintf(p, "%s%ld", _lbl, num);
        lbl = (const char *) p;
    }
};

struct and_lbl_pair_t {
    llvm_label_t start;
    llvm_label_t end;

    void construct() {
        long num = gen_lbl();
        start.construct("and", num);
        end.construct("and_end", num);
    }
};
