#include "alloc.h"
#include "typecheck.h"

long gen_lbl();

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

    void construct(const char *_lbl, long num) {
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

struct lbl_pair_t {
    llvm_label_t start;
    llvm_label_t end;

    void construct(const char *lbl) {
        long num = gen_lbl();
        size_t len = strlen(lbl);
        size_t alloc_size = len + 10;
        char *pstart = (char *) allocate(alloc_size, MEM::FUNC);
        char *pend = (char *) allocate(alloc_size, MEM::FUNC);
        sprintf(pstart, "%s", lbl);
        sprintf(pend, "%s_end", lbl);
        start.construct(pstart, num);
        end.construct(pend, num);
    }
};

struct ExprContext {
    llvm_label_t curr_lbl;
    llvalue_t llval;
};

void set_reg(ssize_t v);
void emit(const char *fmt, ...);
llvalue_t llvm_op_const(int op, int val1, int val2);
void print_const_llvalue(llvalue_t v, bool its_bool);
void print_llvalue(llvalue_t v, bool its_bool);
void print_codegen_indentation();
llvalue_t llvm_op(int op, llvalue_t res1, llvalue_t res2);
llvalue_t llvm_getelementptr(llvalue_t ptr, llvalue_t index);
llvalue_t llvm_load(llvalue_t ptr);
llvalue_t not_llvalue(llvalue_t v);
llvalue_t llvm_calloc(Type *type, llvalue_t sz);
void llvm_gen_lbl(llvm_label_t l);
void llvm_branch_cond(llvalue_t cond, llvm_label_t l1, llvm_label_t l2);
void llvm_branch(llvm_label_t l);
llvalue_t llvm_and_phi(llvm_label_t l1, llvalue_t v1, llvm_label_t l2);
