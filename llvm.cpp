
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include "buf.h"
#include "common.h"
#include "debug_print.h"
#include "llvm.h"
#include "typecheck.h"

ExprContext __expr_context;

// TODO: REMOVE IT
extern config_t config;

static long reg;

void set_reg(ssize_t v) {
    reg = v - 1;
}

static long gen_reg() {
    ++reg;
    return reg;
}

long gen_lbl() {
    static long lbl = 0;
    ++lbl;
    return lbl;
}

int j = 0;

void emit(const char *fmt, ...) {
    if (config.codegen) {
        va_list args;
        va_start(args, fmt);
        print_indentation();
        vprintf(fmt, args);
        va_end(args);
    }
}

llvalue_t llvm_alloca(Type *type) {
    llvalue_t v;
    v.kind = LLVALUE::REG;
    v.reg = gen_reg();
    IdType *idtype = type->is_IdType();
    switch (idtype->kind) {
    case TY::BOOL:
    {
        emit("%%%ld = alloca i1, align 1", v.reg);
    } break;
    case TY::INT:
    {
        emit("%%%ld = alloca i32, align 4", v.reg);
    } break;
    case TY::ARR:
    {
        emit("%%%ld = alloca i32*, align 8", v.reg);
    } break;
    case TY::ID:
    {
        emit("%%%ld = alloca %%class.%s, align 8", v.reg, idtype->id);
    } break;
    default: assert(0);
    }

    return v;
}

llvalue_t llvm_op_const(int op, int val1, int val2) {
    llvalue_t v;
    v.kind = LLVALUE::CONST;
    switch (op) {
    case '+': v.val = val1 + val2; break;
    case '-': v.val = val1 - val2; break;
    case '*': v.val = val1 * val2; break;
    case '<': v.val = val1 < val2; break;
    case '&': v.val = val1 && val2; break;
    default: assert(0);
    }
    return v;
}

void print_const_llvalue(llvalue_t v, bool its_bool) {
    assert(v.kind == LLVALUE::CONST);
    if (its_bool) {
        if (v.val == 0) {
            emit("false");
        } else if (v.val == 1) {
            emit("true");
        } else {
            assert(0 && "Boolean llvalues can only have value 0 or 1");
        }
    } else {
        emit("%d", v.val);
    }
}

void print_llvalue(llvalue_t v, bool its_bool = false) {
    if (v.kind == LLVALUE::CONST) {
        print_const_llvalue(v, its_bool);
    } else {
        emit("%%%ld", v.reg);
    }
}

void print_codegen_indentation() {
    for (int i = 0; i != 4; ++i) {
        emit(" ");
    }
}

llvalue_t llvm_op(int op, llvalue_t res1, llvalue_t res2) {
    llvalue_t v;

    if (res1.kind == LLVALUE::CONST && res2.kind == LLVALUE::CONST) {
        return llvm_op_const(op, res1.val, res2.val);
    }

    v.kind = LLVALUE::REG;

    long lhs = gen_reg();
    v.reg = lhs;
    print_codegen_indentation();
    emit("%%%ld = ", lhs);
    switch (op) {
    case '+': emit("add i32 "); break;
    case '-': emit("sub i32 "); break;
    case '*': emit("mul i32 "); break;
    case '<': emit("icmp slt i32 "); break;
    case '&': emit("and i1 "); break;
    default: assert(0);
    }
    print_llvalue(res1);
    emit(", ");
    print_llvalue(res2);
    emit("\n");
    return v;
}

llvalue_t llvm_bitcast_id_ptr(IdType *type, llvalue_t ptr) {
    assert(ptr.kind == LLVALUE::REG);
    // Assume that `ptr` has type i8*
    llvalue_t v;
    v.reg = gen_reg();
    v.kind = LLVALUE::REG;
    print_codegen_indentation();
    emit("%%%ld = bitcast i8* %%%ld to %%class.%s**\n", v.reg, ptr.reg, type->id);
    return v;
}

llvalue_t llvm_bitcast(Type *type, llvalue_t ptr) {
    llvalue_t v;
    assert(ptr.kind == LLVALUE::REG);
    v.reg = gen_reg();
    v.kind = LLVALUE::REG;
    print_codegen_indentation();
    // Assume that `ptr` has type i8*
    switch (type->kind) {
    case TY::BOOL:
    {
        emit("%%%ld = bitcast i8* %%%ld to i1*\n", v.reg, ptr.reg);
    } break;
    case TY::INT:
    {
        emit("%%%ld = bitcast i8* %%%ld to i32*\n", v.reg, ptr.reg);
    } break;
    case TY::ARR:
    {
        emit("%%%ld = bitcast i8* %%%ld to i32**\n", v.reg, ptr.reg);
    } break;
    // Note: There's no `case TY::ID`. This should be handled by load_bitcast_id_ptr().
    // We could stuff it in here, but it would make this function have a corner case.
    // This load works by passing it the type we want to _load_. But we have no way
    // to represent a type like: pointer to pointer to X. Which is what we need
    // for an object. We load a pointer to an object.
    default: assert(0);
    }
    return v;
}

llvalue_t llvm_getelementptr_i32(llvalue_t ptr, llvalue_t index) {
    // TODO - IMPORTANT: Uncomment
    //assert(ptr.kind == LLVALUE::REG);
    llvalue_t v;
    v.reg = gen_reg();
    v.kind = LLVALUE::REG;
    print_codegen_indentation();
    emit("%%%ld = getelementptr inbounds i32, i32* %%%ld, i32 ", v.reg, ptr.reg);
    print_llvalue(index);
    emit("\n");
    return v;
}

llvalue_t llvm_getelementptr_i8(llvalue_t ptr, size_t offset) {
    llvalue_t v;
    v.reg = gen_reg();
    v.kind = LLVALUE::REG;
    print_codegen_indentation();
    emit("%%%ld = getelementptr inbounds i8, i8* %%%ld, i32 %zd\n", v.reg, ptr.reg, offset);
    return v;
}

llvalue_t llvm_load(Type *type, llvalue_t ptr) {
    // TODO - IMPORTANT: Uncomment
    //assert(ptr.kind == LLVALUE::REG);
    llvalue_t v;
    v.reg = gen_reg();
    v.kind = LLVALUE::REG;
    print_codegen_indentation();
    switch (type->kind) {
    case TY::BOOL:
    {
        emit("%%%ld = load i1, i1* %%%ld, align 1\n", v.reg, ptr.reg);
    } break;
    case TY::INT:
    {
        emit("%%%ld = load i32, i32* %%%ld, align 4\n", v.reg, ptr.reg);
    } break;
    case TY::ARR:
    {
        emit("%%%ld = load i32*, i32** %%%ld, align 8\n", v.reg, ptr.reg);
    } break;
    // Note: There's no `case TY::ID`. This should be handled by load_id_ptr().
    // We could stuff it in here, but it would make this function have a corner case.
    // This load works by passing it the type we want to _load_. But we have no way
    // to represent a type like: pointer to pointer to X. Which is what we need
    // for an object. We load a pointer to an object.
    default: assert(0);
    }
    return v;
}

// Note: I think llvm_load_*() could be one function, where we give
// it a type and loads according to it. The problem is that we don't
// have a way to represent arbitrary pointer types. And we need those
// for pointers to IdTypes. For example, when we do:
//    A a;
//    a = new A();
// this generates an alloca, i.e.:
//    %1 = alloca %class.A*, align 8
// %1 is pointer to pointer to %class.A, and we can't represent that with `Type`.
llvalue_t llvm_load_id_ptr(IdType *type, llvalue_t ptr) {
    llvalue_t v;
    assert(ptr.kind == LLVALUE::REG);
    v.reg = gen_reg();
    v.kind = LLVALUE::REG;
    print_codegen_indentation();
    // Load a pointer to IdType given, from a pointer to pointer to this type.
    emit("%%%ld = load %%class.%s*, %%class.%s** %%%ld, align 8\n", v.reg,
         type->id, type->id, ptr.reg);
    return v;
}

llvalue_t not_llvalue(llvalue_t v) {
    assert(v.kind == LLVALUE::REG);
    long reg = gen_reg();
    print_codegen_indentation();
    emit("%%%ld = icmp eq i1 %%%ld, 0\n", reg, v.reg);
    v.reg = reg;
    return v;
}

llvalue_t llvm_calloc(Type *type, llvalue_t sz) {
    long reg_calloc = gen_reg();
    print_codegen_indentation();
    if (sz.kind == LLVALUE::CONST) {
        emit("%%%ld = call noalias i8* @calloc(i32 1, i32 %d)\n", reg_calloc, sz.val);
    } else {
        emit("%%%ld = call noalias i8* @calloc(i32 1, i32 %%%ld)\n", reg_calloc, sz.reg);
    }
    llvalue_t v;
    v.kind = LLVALUE::REG;
    v.reg = gen_reg();
    // Generate the bitcast
    print_codegen_indentation();
    IdType *idtype = type->is_IdType();
    if (idtype) {
        emit("%%%ld = bitcast i8* %%%ld to %%class.%s*\n", v.reg, reg_calloc, idtype->id);
    } else if (type->kind == TY::ARR) {
        emit("%%%ld = bitcast i8* %%%ld to i32*\n", v.reg, reg_calloc);
    } else {
        assert(0);
    }
    return v;
}

void llvm_gen_lbl(llvm_label_t l) {
    assert(!l.generated);
    emit("%s:\n", l.lbl);
    l.generated = true;
    __expr_context.curr_lbl = l;
}

void llvm_branch_cond(llvalue_t cond, llvm_label_t l1, llvm_label_t l2) {
    print_codegen_indentation();
    emit("br i1 ");
    print_llvalue(cond);
    emit(", label %s, label %s\n\n", l1.lbl, l2.lbl);
}

void llvm_branch(llvm_label_t l) {
    print_codegen_indentation();
    emit("br %s\n\n", l.lbl);
}

llvalue_t llvm_and_phi(llvm_label_t l1, llvalue_t v1, llvm_label_t l2) {
    llvalue_t v;
    v.kind = LLVALUE::REG;
    v.reg = gen_reg();
    // We only need boolean values
    assert(v1.kind == LLVALUE::REG || (v1.val == 0 || v1.val == 1));
    print_codegen_indentation();
    emit("%%%ld = phi i1 [ false, %s ], [ ", v.reg, l1.lbl);
    print_llvalue(v1);
    emit(", %s]\n", l2.lbl);
    return v;
}
