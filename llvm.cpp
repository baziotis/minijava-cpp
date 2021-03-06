
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
    reg = v;
}

long gen_reg() {
    ++reg;
    return reg - 1;
}

static long lbl;

void reset_lbl() {
    lbl = 0;   
}

void set_lbl(long _lbl) {
    lbl = _lbl - 1;   
}

long gen_lbl() {
    ++lbl;
    return lbl;
}

llvalue_t register_llv(long r, LLType type) {
    llvalue_t llval;
    llval.kind = LLVALUE::REG;
    llval.reg = r;
    llval.ty = type;
    return llval;
}

llvalue_t const_llv(int v, LLType type) {
    llvalue_t llval;
    llval.kind = LLVALUE::CONST;
    llval.val = v;
    llval.ty = type;
    return llval;
}

void emit(const char *fmt, ...) {
    if (config.codegen) {
        va_list args;
        va_start(args, fmt);
        vprintf(fmt, args);
        va_end(args);
    }
}

void cgen_init() {
  if (config.codegen) {

    printf(
"declare i8* @calloc(i32, i32)\n"
"declare i32 @printf(i8*, ...)\n"
"declare void @exit(i32)\n"
"\n"
"@_cint = constant [4 x i8] c\"%%d\\0a\\00\"\n"
"@_cOOB = constant [15 x i8] c\"Out of bounds\\0a\\00\"\n"
"define void @print_int(i32 %%i) {\n"
"    %%_str = bitcast [4 x i8]* @_cint to i8*\n"
"    call i32 (i8*, ...) @printf(i8* %%_str, i32 %%i)\n"
"    ret void\n"
"}\n"
"\n"
"define void @throw_oob() {\n"
"    %%_str = bitcast [15 x i8]* @_cOOB to i8*\n"
"    call i32 (i8*, ...) @printf(i8* %%_str)\n"
"    call void @exit(i32 1)\n"
"    ret void\n"
"}\n\n"
);

  }
}

llvalue_t llvm_op_const(int op, int val1, int val2) {
    int val = 0;
    switch (op) {
    case '+': val = val1 + val2; break;
    case '-': val = val1 - val2; break;
    case '*': val = val1 * val2; break;
    case '<': val = val1 < val2; break;
    case '&': val = val1 && val2; break;
    default: assert(0);
    }
    return const_llv(val, {});
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

// TODO: We should probably pass the type here.
void cgen_print_llvalue(llvalue_t v, bool its_bool /* = false */) {
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

void cgen_print_stmt(llvalue_t to_print) {
  print_codegen_indentation();
  emit("call void @print_int(i32 ");
  cgen_print_llvalue(to_print);
  emit(")\n");
}

llvalue_t llvm_op(int op, llvalue_t res1, llvalue_t res2) {
    if (res1.kind == LLVALUE::CONST && res2.kind == LLVALUE::CONST) {
        return llvm_op_const(op, res1.val, res2.val);
    }

    long lhs = gen_reg();
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
    cgen_print_llvalue(res1);
    emit(", ");
    cgen_print_llvalue(res2);
    emit("\n");
    return register_llv(lhs, {});
}

void cgen_print_lltype(Type *type) {
    switch (type->kind) {
    case TY::BOOL: emit("i1"); break;
    case TY::INT: emit("i32"); break;
    case TY::ARR: emit("i32*"); break;
    case TY::ID:
    {
        emit("i8*");
    } break;
    default: assert(0);
    }
}

void emit_vmethod_signature(Type *base_obj_ty, Method *method) {
    cgen_print_lltype(method->ret_type);
    // Print "this" pointer
    emit(" (i8*");
    //cgen_print_lltype(base_obj_ty);
    if (method->param_len) {
      emit(", ");
    }
    for (size_t i = 0; i != method->param_len; ++i) {
        Local *param = method->locals[i];
        cgen_print_lltype(param->type);
        if (i != method->param_len - 1) {
            emit(", ");
        }
    }
    emit(")*");
}

llvalue_t cgen_get_virtual_method(Type *base_obj_ty, llvalue_t base_obj, Method *method) {
    assert(base_obj.kind == LLVALUE::REG);
    long i8ppp = gen_reg();
    print_codegen_indentation();
    // Bitcast the object pointer to i8***.
    // Note that the vptr is a i8**. So, with this bitcast,
    // and just loading a i8**, we get the vptr.
    emit("%%%ld = bitcast i8* ", i8ppp);
    emit("%%%ld to i8***\n", base_obj.reg);
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
        emit("%%%ld = getelementptr i8*, i8** %%%ld, i32 %zd\n",
             gep, vptr, offset / 8);
    }
    long vmethod_i8 = gen_reg();
    print_codegen_indentation();
    // Load the pointer for the method.
    emit("%%%ld = load i8*, i8** %%%ld\n", vmethod_i8, gep);
    llvalue_t vmethod = register_llv(gen_reg(), {});
    print_codegen_indentation();
    // Finally, bitcast this pointer to the type of the method.
    emit("%%%ld = bitcast i8* %%%ld to ", vmethod.reg, vmethod_i8);
    emit_vmethod_signature(base_obj_ty, method);
    emit("\n");
    return vmethod;
}

llvalue_t cgen_get_field_ptr(Local *field) {
    assert(field->kind == (int)LOCAL_KIND::FIELD);
    llvalue_t reg0 = register_llv(0, {});
    llvalue_t ptr = reg0;
    int account_for_vptr = field->offset + 8;
    ptr = llvm_getelementptr_i8(reg0, account_for_vptr);
    return llvm_bitcast_from_i8p(field->type, ptr);
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
        emit("%%%ld = bitcast i8* %%%ld to i8**\n", v.reg, ptr.reg);
    } break;
    default: assert(0);
    }
    return v;
}

llvalue_t llvm_bitcast_i8p_to_i8ppp(llvalue_t i8p) {
  llvalue_t i8ppp = register_llv(gen_reg(), {});
  print_codegen_indentation();
  emit("%%%ld = bitcast i8* %%%ld to i8***\n", i8ppp.reg, i8p.reg);
  return i8ppp;
}


void cgen_store_vptr(llvalue_t i8ppp, IdType *type) {
  llvalue_t i8pp_to_vtable = register_llv(gen_reg(), {});
  print_codegen_indentation();
  emit("%%%ld = bitcast [%ld x i8*]* @.%s to i8**\n", i8pp_to_vtable.reg,
       type->vmethods_len, type->id);
  print_codegen_indentation();
  emit("store i8** %%%ld, i8*** %%%ld\n", i8pp_to_vtable.reg, i8ppp.reg);
}

llvalue_t llvm_getelementptr_i32(llvalue_t ptr, llvalue_t index) {
    assert(ptr.kind == LLVALUE::REG);
    llvalue_t v;
    v.reg = gen_reg();
    v.kind = LLVALUE::REG;
    print_codegen_indentation();
    emit("%%%ld = getelementptr inbounds i32, i32* %%%ld, i32 ", v.reg, ptr.reg);
    cgen_print_llvalue(index);
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
        emit("%%%ld = load i8*, i8** %%%ld\n", v.reg, ptr.reg);
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

llvalue_t llvm_calloc(llvalue_t sz) {
    llvalue_t v = register_llv(gen_reg(), {});
    print_codegen_indentation();
    if (sz.kind == LLVALUE::CONST) {
        emit("%%%ld = call noalias i8* @calloc(i32 1, i32 %d)\n", v.reg, sz.val);
    } else {
        emit("%%%ld = call noalias i8* @calloc(i32 1, i32 %%%ld)\n", v.reg, sz.reg);
    }
    return v;
}

void llvm_gen_lbl(llvm_label_t l) {
    emit("%s:\n", l.lbl);
    __expr_context.curr_lbl = l;
}

void llvm_branch_cond(llvalue_t cond, llvm_label_t l1, llvm_label_t l2) {
    print_codegen_indentation();
    emit("br i1 ");
    cgen_print_llvalue(cond);
    emit(", label %%%s, label %%%s\n\n", l1.lbl, l2.lbl);
}

void llvm_branch(llvm_label_t l) {
    print_codegen_indentation();
    emit("br label %%%s\n\n", l.lbl);
}

llvalue_t llvm_phi_node(Type *type, llvalue_t val1,
                    llvalue_t val2, llvm_label_t lbl1,
                    llvm_label_t lbl2, long reg) {
    llvalue_t phi_value;
    phi_value.kind = LLVALUE::REG;
    if (reg == -1) {
        phi_value.reg = gen_reg();
    } else {
        phi_value.reg = reg;
    }
    print_codegen_indentation();
    emit("%%%ld = phi ", phi_value.reg);
    cgen_print_lltype(type);
    emit(" [ ");
    cgen_print_llvalue(val1);
    emit(", %%%s ], [ ", lbl1.lbl);
    cgen_print_llvalue(val2);
    emit(", %%%s ]\n", lbl2.lbl);
    return phi_value;
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
    cgen_print_lltype(ret_type);
    emit(" ");  // space
    assert(vmethod.kind == LLVALUE::REG);
    // Print the register that points to the vmethod
    emit("%%%ld(i8* ", vmethod.reg);
    cgen_print_llvalue(base_obj);
    if (types.len) {
      emit(", ");
    }
    // Print the types of args along with the args themselves
    // as a comma-separate list
    assert(types.len == values.len);
    for (size_t i = 0; i != types.len; ++i) {
        cgen_print_lltype(types[i]);
        emit(" ");  // space
        cgen_print_llvalue(values[i]);
        if (i != types.len - 1) {
            emit(", ");
        }
    }
    emit(")\n");
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
    // We don't actually want to assert here because we might have an undefined
    // type. The error has been issued elsewhere.
    default: break;
    }
    return v;
}

void llvm_store(Type *type, llvalue_t value, llvalue_t ptr) {
    assert(ptr.kind == LLVALUE::REG);
    print_codegen_indentation();
    emit("store ");
    cgen_print_lltype(type);
    emit(" ");
    cgen_print_llvalue(value);
    emit(", ");
    cgen_print_lltype(type);
    emit("* ");
    cgen_print_llvalue(ptr);
    emit("\n");
}

void cgen_start_method(Method *method, const char *class_name, bool is_main_method) {
    reset_lbl();
    // Emit function prototype
    emit("define ");
    cgen_print_lltype(method->ret_type);

    if (!is_main_method) {
      emit(" @%s__%s(", class_name, method->id);
    } else {
      emit(" @main(", class_name, method->id);
    }
    
    // Emit `this` pointer.
    size_t param_len = method->param_len;
    emit("i8* %%0");
    if (param_len)
      emit(", ");
    

    // Initialize parameters with registers and also
    // print them as part of the method prototype.
    // We're starting from 1, because register 0
    // is always reserved for the 'this' pointer.
    // Set a register _only_ for the parameters.
    size_t local_reg_counter = 1;
    size_t param_counter;
    for (param_counter = 0; param_counter < param_len; ++param_counter) {
        Local *local = method->locals[param_counter];
        // Print
        cgen_print_lltype(local->type);
        emit(" %%%ld", local_reg_counter);
        if (param_counter != param_len - 1) {
          emit(", ");
        }
        // Initialize
        local->kind = (int)LOCAL_KIND::PARAM;
        local->llval = register_llv(local_reg_counter, {});
        local->index = param_counter;
        ++local_reg_counter;
    }
    set_reg(local_reg_counter);

    // Start of body of method
    emit(") {\n");

    // Initialize locals. Locals initially get a
    // register that is a pointer to some memory
    // that can hold the value. It's only when they're
    // first used that the value is loaded into a reg.
    // So, initialize them with this pointer and also
    // generate allocas.
    size_t locals_len = method->locals.len;
    size_t var_counter = param_counter;
    for (; var_counter < locals_len; ++var_counter) {
        Local *local = method->locals[var_counter];
        local->kind = (int)LOCAL_KIND::VAR;
        local->index = var_counter;
        // Initialize all the values to 0.
        local->llval = const_llv(0, {});
    }

    llvm_label_t entry_lbl = llvm_label_t("entry");
    llvm_gen_lbl(entry_lbl);
}

void cgen_end_method(bool is_main_method) {
    if (is_main_method) {
      emit("    ret i32 0\n");
    }
    emit("}\n\n");
}

llvalue_t cgen_cast_value(llvalue_t value, Type *from_ty, Type *to_ty) {
    long reg = gen_reg();
    print_codegen_indentation();
    emit("%%%ld = bitcast ", reg);
    cgen_print_lltype(from_ty);
    emit(" ");
    cgen_print_llvalue(value);
    emit(" to ");
    cgen_print_lltype(to_ty);
    emit("\n");
    value.kind = LLVALUE::REG;
    value.reg = reg;
    return value;
}

void llvm_ret(Type *ty, llvalue_t val) {
    print_codegen_indentation();
    emit("ret ");
    cgen_print_lltype(ty);
    emit(" ");
    cgen_print_llvalue(val);
    emit("\n");
}
