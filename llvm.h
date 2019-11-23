#include "alloc.h"
#include "typecheck.h"

long gen_lbl();

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
            num = gen_lbl();
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

struct TrackAssignment {
    llvalue_t llval;
    bool assigned;
};

typedef Buf<TrackAssignment> TrackAssignmentBuf;

struct ExprContext {
    llvm_label_t curr_lbl;
    llvalue_t llval;
    // Saved by EXPR::ARR_ALLOC and
    // used only in AssignmentStatement only for arrays.
    llvalue_t len;

    Method *method;

    int nesting_level;
    int max_nesting_level;
    bool in_if;
    // One buf for every nesting level
    Buf<TrackAssignmentBuf> if_bufs;
    Buf<TrackAssignmentBuf> else_bufs;
};


void cgen_print_llvalue(llvalue_t v, bool its_bool = false);
void cgen_print_lltype(Type *type);
long gen_reg();
void set_reg(ssize_t v);
void reset_lbl();
void set_lbl(long _lbl);
void emit(const char *fmt, ...);
llvalue_t llvm_op_const(int op, int val1, int val2);
llvalue_t llvm_op(int op, llvalue_t res1, llvalue_t res2);
llvalue_t llvm_bitcast_from_i8p(Type *type, llvalue_t ptr);
llvalue_t cgen_get_virtual_method(Type *base_obj_ty, llvalue_t base_obj, Method *method);
llvalue_t cgen_get_field_ptr(Local *field);
llvalue_t llvm_getelementptr_i32(llvalue_t ptr, llvalue_t index);
llvalue_t llvm_getelementptr_i8(llvalue_t ptr, size_t offset);
llvalue_t llvm_load(Type *type, llvalue_t ptr);
llvalue_t not_llvalue(llvalue_t v);
llvalue_t llvm_calloc(Type *type, llvalue_t sz);
void llvm_gen_lbl(llvm_label_t l);
void llvm_branch_cond(llvalue_t cond, llvm_label_t l1, llvm_label_t l2);
void llvm_branch(llvm_label_t l);
llvalue_t llvm_and_phi(llvm_label_t l1, llvalue_t v1, llvm_label_t l2);
llvalue_t llvm_call(Type *ret_type, Type *base_obj_ty, llvalue_t base_obj,
                    llvalue_t vmethod, FuncArr<Type*> types,
                    FuncArr<llvalue_t> values);
llvalue_t llvm_alloca(Type *type);
void llvm_store(Type *type, llvalue_t value, llvalue_t ptr);
void cgen_start_method(Method *method, const char *class_name);
void cgen_end_method();
llvalue_t cgen_cast_value(llvalue_t value, Type *from_ty, Type *to_ty);
void llvm_ret(Type *ty, llvalue_t val);
