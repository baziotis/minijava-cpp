
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

long gen_reg() {
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

static void print_const_llvalue(llvalue_t v, bool its_bool) {
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

void print_llvalue(llvalue_t v, bool its_bool /* = false */) {
    if (v.kind == LLVALUE::CONST) {
        print_const_llvalue(v, its_bool);
    } else {
        emit("%%%ld", v.reg);
    }
}

static void print_codegen_indentation() {
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

void print_lltype(Type *type) {
    switch (type->kind) {
    case TY::BOOL: emit("i1"); break;
    case TY::INT: emit("i32"); break;
    case TY::ARR: emit("i32*"); break;
    case TY::ID:
    {
        IdType *idtype = (IdType*) type;
        assert(idtype->is_IdType());
        emit("%%class.%s*", idtype->id);
    } break;
    default: assert(0);
    }
}

llvalue_t get_virtual_method(Type *base_obj_ty, llvalue_t base_obj, Method *method) {
    assert(base_obj.kind == LLVALUE::REG);
    long i8ppp = gen_reg();
    print_codegen_indentation();
    // Bitcast the object pointer to i8***.
    // Note that the vptr is a i8**. So, with this bitcast,
    // and just loading a i8**, we get the vptr.
    emit("%%%ld = bitcast ", i8ppp);
    print_lltype(base_obj_ty);
    emit(" %%%ld to i8***\n", base_obj.reg);
    long vptr = gen_reg();
    print_codegen_indentation();
    // Get the vptr
    emit("%%%ld = load i8**, i8*** %%%ld, align 8\n", vptr, i8ppp);
    // Go to the correct offset in the virtual table.
    long gep = vptr;
    size_t offset = method->offset;
    if (offset) {
        gep = gen_reg();
        print_codegen_indentation();
        // offset / 8 because the type is i8**. So, a pointer
        // and because GEP works like C pointers, +1 in a pointer
        // is +8 actual.
        emit("%%%ld = getelementptr i8*, i8** %%%ld, %zd\n",
             gep, vptr, offset / 8);
    }
    long vmethod_i8 = gen_reg();
    print_codegen_indentation();
    // Load the pointer for the method.
    emit("%%%ld = load i8*, i8** %%%ld\n", vmethod_i8, gep);
    llvalue_t vmethod;
    vmethod.kind = LLVALUE::REG;
    vmethod.reg = gen_reg();
    print_codegen_indentation();
    // Finally, bitcast this pointer to the type of the method.
    emit("%%%ld = bitcast i8* %%%ld to (", vmethod.reg, vmethod_i8);
    for (size_t i = 0; i != method->param_len; ++i) {
        Local *param = method->locals[i];
        print_lltype(param->type);
        if (i != method->param_len - 1) {
            emit(", ");
        }
    }
    emit(")*\n");
    return vmethod;
}

llvalue_t llvm_bitcast_from_i8p(Type *type, llvalue_t ptr) {
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
    case TY::ID:
    {
        IdType *idtype = (IdType*) type;
        assert(idtype->is_IdType());
        emit("%%%ld = bitcast i8* %%%ld to %%class.%s**\n", v.reg, ptr.reg, idtype->id);
    } break;
    default: assert(0);
    }
    return v;
}

llvalue_t llvm_getelementptr_i32(llvalue_t ptr, llvalue_t index) {
    assert(ptr.kind == LLVALUE::REG);
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
    assert(ptr.kind == LLVALUE::REG);
    llvalue_t v;
    v.reg = gen_reg();
    v.kind = LLVALUE::REG;
    print_codegen_indentation();
    switch (type->kind) {
    case TY::BOOL:
    {
        emit("%%%ld = load i1, i1* %%%ld\n", v.reg, ptr.reg);
    } break;
    case TY::INT:
    {
        emit("%%%ld = load i32, i32* %%%ld, align 4\n", v.reg, ptr.reg);
    } break;
    case TY::ARR:
    {
        emit("%%%ld = load i32*, i32** %%%ld, align 8\n", v.reg, ptr.reg);
    } break;
    case TY::ID:
    {
        IdType *idtype = (IdType*) type;
        assert(idtype->is_IdType());
        emit("%%%ld = load %%class.%s*, %%class.%s** %%%ld, align 8\n", v.reg,
             idtype->id, idtype->id, ptr.reg);
    } break;
    default: assert(0);
    }
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

llvalue_t llvm_call(Type *ret_type, Type *base_obj_ty, llvalue_t base_obj,
                    llvalue_t vmethod, FuncArr<Type*> types,
                    FuncArr<llvalue_t> values)
{
    llvalue_t v;
    v.kind = LLVALUE::REG;
    v.reg = gen_reg();
    print_codegen_indentation();
    emit("%%%ld = call ", v.reg);
    // Print the return type
    print_lltype(ret_type);
    emit(" ");  // space
    assert(vmethod.kind == LLVALUE::REG);
    // Print the register that points to the vmethod
    emit("%%%ld(", vmethod.reg);
    // Pass base object as first implicit param.
    print_lltype(base_obj_ty);
    emit(" ");
    print_llvalue(base_obj);
    emit(", ");
    // Print the types of args along with the args themselves
    // as a comma-separate list
    assert(types.len == values.len);
    for (size_t i = 0; i != types.len; ++i) {
        print_lltype(types[i]);
        emit(" ");  // space
        print_llvalue(values[i]);
        if (i != types.len - 1) {
            emit(", ");
        }
    }
    emit(")");
    return v;
}

llvalue_t llvm_alloca(Type *type) {
    llvalue_t v;
    v.reg = gen_reg();
    v.kind = LLVALUE::REG;
    print_codegen_indentation();
    switch (type->kind) {
    case TY::BOOL:
    {
        emit("%%%ld = alloca i1\n", v.reg);
    } break;
    case TY::INT:
    {
        emit("%%%ld = alloca i32, align 4\n", v.reg);
    } break;
    case TY::ARR:
    {
        emit("%%%ld = alloca i32*, align 8\n", v.reg);
    } break;
    case TY::ID:
    {
        IdType *idtype = (IdType*) type;
        assert(idtype->is_IdType());
        emit("%%%ld = alloca %%class.%s*, align 8\n", v.reg, idtype->id);
    } break;
    default: assert(0);
    }
    return v;
}

void llvm_store(Type *type, llvalue_t value, llvalue_t ptr) {
    assert(ptr.kind == LLVALUE::REG);
    print_codegen_indentation();
    emit("store ");
    print_lltype(type);
    emit(" ");
    print_llvalue(value);
    emit(", ");
    print_lltype(type);
    emit("* ");
    print_llvalue(ptr);
    emit("\n");
}
