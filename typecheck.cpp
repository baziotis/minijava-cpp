#include <assert.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include "ast.h"
#include "alloc.h"
#include "array.h"
#include "common.h"
#include "debug_print.h"
#include "error.h"          // log()
                            // WARNING: error.h gives access to the global `loc`
#include "hash_table.h"
#include "str_intern.h"

extern config_t config;

void typecheck_init() {
    set_indent_char('*');
}

/// Fill the type_table.
TypeTable install_type_declarations(Goal *goal) {
    DeclarationVisitor decl_visitor(goal->type_decls.len);
    goal->accept(&decl_visitor);

    // Free memory that was used for allocating objects
    // for parsing that doenot persist in pass 2 of type-checking.
    deallocate(MEM::PARSE_TEMP);

    return decl_visitor.type_table;
}

void full_typecheck(Goal *goal, TypeTable type_table) {
    MainTypeCheckVisitor main_visitor(type_table);
    goal->accept(&main_visitor);
}

void TypeTable::compute_and_print_offsets_for_type(IdType *type) {
    assert(type);
    if (type->state == STATE::RESOLVING || type->state == STATE::RESOLVED) {
        // If in STATE::RESOLVING, we have cyclic inheritance.
        // We issue error elsewhere. Otherwise, we have already
        // resolved this type.
        return;
    }
    IdType *parent = type->parent;
    size_t start_fields = 0;
    size_t start_methods = 0;
    if (parent) {
        compute_and_print_offsets_for_type(parent);
        start_fields = parent->fields_end;
        start_methods = parent->methods_end;
    }
    
    size_t running_offset = start_fields;
    for (Local *field : type->fields) {
        printf("%s.%s: %zd\n", type->id, field->id, running_offset);
        if (field->type == this->bool_type) {
            running_offset += 1;
        } else if (field->type == this->int_type) {
            running_offset += 4;
        } else if (field->type == this->int_arr_type ||
                   field->type->is_IdType()) {
            running_offset += 8;
        }
    }

    size_t fields_size = running_offset - start_fields;
    

    running_offset = start_methods;
    for (Method *method: type->methods) {
        if (!method->overrides) {
            printf("%s.%s: %zd\n", type->id, method->id, running_offset);
            running_offset += 8;
        }
    }

    size_t methods_size = running_offset - start_methods;

    type->fields_end = start_fields + fields_size;
    type->methods_end = start_methods + methods_size;
    type->state = STATE::RESOLVED;
}

void TypeTable::offset_computation() {
    for (IdType *type : type_table) {
        compute_and_print_offsets_for_type(type);
    }
}

void typecheck(Goal goal) {
    typecheck_init();
    // Pass 1
    TypeTable type_table = install_type_declarations(&goal);
    if (config.log) {
        printf("\n");
    }
    // TODO: How able we are to continue?
    // Print types that could not be inserted. This can
    // happen if the types used are more than the type
    // declarations. See TypeTable.
    for (IdType *type : type_table.could_not_be_inserted) {
        typecheck_error(type->loc, "Type `", type->id, "` has not been ",
                        "defined");
    }
    // Pass 2
    full_typecheck(&goal, type_table);
    
    if (config.offsets) {
        // Offset computation
        type_table.offset_computation();
    }
}


/* Declaration Visitor
 */
IdType* DeclarationVisitor::id_to_type(const char *id, location_t loc) {
    // Check if it already exists.
    IdType *type = this->type_table.find(id);
    if (type) {
        return type;
    }
    // Otherwise construct a new one and
    // save it in the table.
    type = new IdType(id);
    type->loc = loc;
    this->type_table.insert(type->id, type);
    return type;
}

// This is member of the visitor so that we have
// access to the type table.
Type* DeclarationVisitor::typespec_to_type(Typespec tspec, location_t loc) {
    switch (tspec.kind) {
    case TYSPEC::UNDEFINED: assert(0);
    case TYSPEC::INT: return type_table.int_type;
    case TYSPEC::ARR: return type_table.int_arr_type;
    case TYSPEC::BOOL: return type_table.bool_type;
    case TYSPEC::ID: return this->id_to_type(tspec.id, loc);
    default: assert(0);
    }
}

/* Type Table
 */
void TypeTable::initialize(size_t n) {
    type_table.reserve(n);
    undefined_type = new Type(TY::UNDEFINED);
    bool_type = new Type(TY::BOOL);
    int_type = new Type(TY::INT);
    int_arr_type = new Type(TY::ARR);
}


void TypeTable::insert(const char *id, IdType* v) {
    if (!type_table.insert(id, v)) {
        // Search in the auxiliary buffer
        for (IdType *type : could_not_be_inserted) {
            if (type->id == id) {
                return;
            }
        }
        could_not_be_inserted.push(v);
    }
}

DeclarationVisitor::DeclarationVisitor(size_t ntype_decls) {
    // IMPORTANT: Before installing a type, the user
    // is responsible for checking if it exists.
    this->type_table.initialize(ntype_decls);
}

void DeclarationVisitor::visit(Goal *goal) {
    LOG_SCOPE;
    debug_print("Pass1::Goal\n");
    goal->main_class.accept(this);
    for (TypeDeclaration *type_decl : goal->type_decls) {
        type_decl->accept(this);
    }
}

void DeclarationVisitor::visit(MainClass *main_class) {
    LOG_SCOPE;
    debug_print("Pass1::MainClass: %s\n", main_class->id);
}

// Note - IMPORTANT: 
// A lot of IdTypes just start undefined (i.e. kind == TY::UNDEFINED)
// if we have seen a declaration with this type but we have
// not yet seen a definition. For example:
//     int test(A a)
// if we have not yet have seen the definition of A.
// We install (i.e allocate) a type (if one does not exist already for id `A`)
// but we just keep it undefined (and keep a pointer to it).
// If we then _see_
//     class A { ...
// we don't allocate a new type (note below that we're searching the table).
// We do the changes in the same memory that was allocated when we were in `test(A a)`.
// And we change the type from undefined.

// If we never see the definition of `A`, it will be caught in pass 2 as described below.

// Essentially, this almost eliminates searches in the type table in the pass 2.
// We just check the pointer and if the type was never defined, it will have
// remained undefined and we issue an error. Otherwise, the definition of this
// type will be in the same memory location.

void DeclarationVisitor::visit(TypeDeclaration *type_decl) {
    LOG_SCOPE;
    assert(!type_decl->is_undefined());
    print_indentation();
    debug_log(type_decl->loc, "Pass1::TypeDeclaration: ", type_decl->id, "\n");
    IdType *type = this->type_table.find(type_decl->id);
    // If it exists and it's declared (i.e. we have processed
    // a type with the same `id`), then we have redeclaration error.
    if (type) {
        if (type->is_defined()) {
            typecheck_error(type_decl->loc, "Type with id: `", type_decl->id,
                            "` has already been declared");
            return;
        } else {
            // Overwrite the location. If the type is already inserted, its
            // loc is its first usage.
            type->loc = type_decl->loc;
            type->set_sizes(type_decl->vars.len, type_decl->methods.len);
        }
    } else {
        type = new IdType(type_decl->id, type_decl->vars.len, type_decl->methods.len);
        type->loc = type_decl->loc;
        // TODO: insert it as undefined if it's not correct.
        this->type_table.insert(type->id, type);
    }
    // Handle inheritance
    if (type_decl->extends) {
        IdType *parent = this->id_to_type(type_decl->extends, type_decl->loc);
        type->set_parent(parent);
    }
    for (LocalDeclaration *ld : type_decl->vars) {
        // Check redefinition in fields.
        // IMPORTANT:
        // - A field must not conflict with a field of the current class.
        // - A field can conflict with a field of the parent class and
        //   this is called shadowing. And it's correct (when we refer
        //   to the field, we refer to the most immediate in the
        //   inheritance tree).
        // - A field can't possibly conflict with a method
        //   of the current class because first we define all the fields,
        //   then all the methods (so, only a method can conflict with a field).
        // - A field can conflict with a method of a parent class, but that's
        //   NOT an error. Because it is disambiguated in what we refer to
        //   by the way we dereference.

        Field *field = type->fields.find(ld->id);
        if (field) {
            typecheck_error(ld->loc, "In class with id: `", type_decl->id,
                            "`, redefinition of field with id: `", field->id, "`");
        } else {
            field = ld->accept(this);
            // All fields are considered initialized.
            field->initialized = true;
            //field->type->print();
            type->fields.insert(field->id, field);
        }
    }
    for (MethodDeclaration *md : type_decl->methods) {
        // Check redefinition
        // IMPORTANT:
        // - A method must not conflict with a method of the current class.
        // - A method must not conflict with a field of the current class.
        // - A method can conflict with a method of the parent class:
        //   -- If the methods have the same return type (but possibly different formal
        //      parameters), it's valid overriding.Otherwise,
        //   -- Otherwise, if the methods have different return types but the types
        //      of the parameters don't match _exactly_, then it's valid
        //      overriding.
        //   -- Otherwise, it's an error.
        //   We CAN'T check this in the first pass as it is possible that
        //   we have not processed the parent type yet.
        // - A method can conflict with a field of the parent 
        //   but that's NOT an error.  Because it is disambiguated in what
        //   we refer to by the way we dereference.

        Method *method = type->methods.find(md->id);
        if (method) {
            typecheck_error(md->loc, "In class with id: `", type_decl->id,
                            "`, redefinition of method with id: `", method->id,
                            "`. Note that you can override but not overload ",
                            "a method");
        } else {
            Field *field = type->fields.find(md->id);
            if (field) {
                typecheck_error(md->loc, "In class with id: `", type_decl->id,
                                "`, redefinition of method with id: `",
                                md->id, "`. A field with the same id ",
                                "has already been defined");
            } else {
                method = md->accept(this);
                //method->print();
                assert(method);
                assert(method->id);
                type->methods.insert(method->id, method);
            }
        }
    }
}

Local *DeclarationVisitor::visit(LocalDeclaration *local_decl) {
    // Note: It's responsibility of the one who calls this visit
    // to have assured that a local declaration with the same
    // id does not exist.
    LOG_SCOPE;
    assert(!local_decl->is_undefined());
    print_indentation();
    debug_log(local_decl->loc, "LocalDeclaration: ", local_decl->id, "\n");
    Type *type = typespec_to_type(local_decl->typespec, local_decl->loc);
    Local *local = new Local(local_decl->id, type);
    return local;
}

const char *DeclarationVisitor::gen_id() {
    static int count = 0;
    char buf[64];
    sprintf(buf, "a%d", count);
    count++;
    return str_intern(buf);
}

Method *DeclarationVisitor::visit(MethodDeclaration *method_decl) {
    // Note: It's responsibility of the one who calls this visit
    // to have assured that a method with the same id does not exist.
    LOG_SCOPE;
    assert(!method_decl->is_undefined());
    print_indentation();
    debug_log(method_decl->loc, "MethodDeclaration: ", method_decl->id, "\n");
    Method *method = new Method(method_decl);
    assert(method);
    method->ret_type = this->typespec_to_type(method_decl->typespec, method_decl->ret->loc);
    for (LocalDeclaration *par : method_decl->params) {
        // We accept the param anyway. But in the case that there is
        // another param with the same name, we still insert this
        // (so that the rest of type-checking can sort of continue) but
        // we have to generate a (dummy) id for it.
        Param *param = par->accept(this);
        if (method->locals.find(par->id)) {
            typecheck_error(par->loc, "Parameter `", par->id, "` is already defined",
                            " in method `", method_decl->id, "`");
            param->id = gen_id();
        }
        // All params are considered initialized.
        param->initialized = true;
        method->locals.insert(param->id, param);
    }
    for (LocalDeclaration *var : method_decl->vars) {
        if (method->locals.find(var->id)) {
            typecheck_error(var->loc, "Variable `", var->id, "` is already defined",
                            " in method `", method_decl->id, "`");
        } else {
            Var *v = var->accept(this);
            method->locals.insert(v->id, v);
        }
    }
    return method;
}

/* Main TypeCheck Visitor (Pass 2)
 */
void MainTypeCheckVisitor::visit(Goal *goal) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::Goal\n");
    goal->main_class.accept(this);
    for (IdType *type : this->type_table) {
        if (type->is_defined()) {
            type->accept(this);
        } else {
            // type->loc is going to be the location of its first usage (Check
            // id_to_type()). Ideally, we would like to have all the locations
            // it was used, or some, but this will at least make this a not very
            // bad message.
            typecheck_error(type->loc, "Type `", type->id, "` has not been ",
                            "defined");
        }
    }
}

void MainTypeCheckVisitor::visit(MainClass *main_class) {
    LOG_SCOPE;
    //debug_print("MainTypeCheck::MainClass\n");
}

static Method *lookup_method_parent(const char *id, IdType *cls, IdType **ret_parent = NULL) {
    IdType *runner = cls;
    while (runner->parent) {
        runner = runner->parent;
        Method *method = runner->methods.find(id);
        if (method) {
            *ret_parent = runner;
            return method;
        }
        // Cyclic inheritance, we issue error elsewhere.
        if (runner == cls) break;
    }
    return NULL;
}

static Method *lookup_method(const char *id, IdType *cls) {
    // Check current class's methods
    Method *method = cls->methods.find(id);
    if (method) {
        return method;
    }
    // Check parent's methods (account for cyclic inheritance).
    return lookup_method_parent(id, cls);
}

static bool params_match(Method *m1, Method *m2) {
    if (m1->param_len != m2->param_len) return false;
    size_t it = 0;
    for (Local *p1 : m1->locals) {
        Local *p2 = m2->locals[it];
        if (p1->type != p2->type) return false;
        if (it == m1->param_len) break;
    }
    return true;
}

static void print_param_types(Method *method) {
    size_t param_len = method->param_len;
    for (size_t i = 0; i != param_len; ++i) {
        Local *param = method->locals[i];
        param->type->print();
        if (i + 1 != param_len) {
            printf(", ");
        }
    }
}

void MainTypeCheckVisitor::visit(IdType *type) {
    LOG_SCOPE;
    assert(type->is_IdType());
    assert(type->is_defined());
    debug_print("MainTypeCheck::IdType %s\n", type->id);

    // Cyclic inheritance detection
    IdType *runner = type;
    while (runner->parent) {
        runner = runner->parent;
        if (runner == type) {
            typecheck_error(type->loc, "Cyclic inheritance ",
                            "involving `", type->id, "`");
            break;
        }
    }

    this->curr_class = type;
    for (Method *method : type->methods) {
        IdType *parent;
        Method *method_parent = lookup_method_parent(method->id, type, &parent);
        if (method_parent) {
            bool match = params_match(method, method_parent);
            if (method_parent->ret_type != method->ret_type) {
                // Note: If they _match exactly_ it is an error. Otherwise,
                // it can be disambiguated through the parameters.
                if (match) {
                    typecheck_error_no_ln(method->loc, method->id, "(");
                    print_param_types(method);
                    printf(") in `%s` can't override %s(", type->id, method_parent->id);
                    print_param_types(method_parent);
                    printf(") in `%s`\n", parent->id);
                    printf("  Mismatch in return types `%s` and `%s`\n",
                           method->ret_type->name(), method_parent->ret_type->name());
                }
            } else if (match) {
                method->overrides = true;
            }
        }
        method->accept(this);
    }
    this->curr_class = NULL;
}
static long reg;

static void set_reg(ssize_t v) {
    reg = v - 1;
}

static long gen_reg() {
    ++reg;
    return reg;
}

static long gen_lbl() {
    static long lbl = 0;
    ++lbl;
    return lbl;
}

// True if `rhs` is child of `lhs` (or they're equal)
static bool compatible_types(Type *lhs, Type *rhs) {
    if (lhs == rhs) return true;
    IdType *ty1 = lhs->is_IdType();
    IdType *ty2 = rhs->is_IdType();
    if (ty1 && ty2) {
        IdType *runner = ty2;
        while (runner->parent) {
            runner = runner->parent;
            if (runner == ty1) {
                return true;
            }
            // Cyclic inheritance, we issue error elsewhere.
            if (runner == ty2) break;
        }
        return false;
    }
    return false;
}

#include "llvm.h"

struct {
    llvm_label_t curr_lbl;
    llvalue_t llval;
    Type *ty;
} __expr_context;

void MainTypeCheckVisitor::visit(Method *method) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::Method %s\n", method->id);
    this->curr_method = method;
    // We're starting from 1, because register 0
    // is always reserved for the 'this' pointer.
    ssize_t param_counter = 1;
    for (Local *local : method->locals) {
        local->reg = param_counter;
        ++param_counter;
    }
    set_reg(param_counter);

    // TODO: Emit the entry label / basic block for each
    // function
    // Set current label to %0
    __expr_context.curr_lbl = llvm_label_t("entry");

    for (Statement *stmt : method->stmts) {
        stmt->accept(this);
    }
    // An undefined return expression may actually end up here.
    // Check parse.cpp
    if (!method->ret_expr->is_undefined()) {
        assert(method->ret_expr);
        Type *ret_type = method->ret_expr->accept(this);
        assert(ret_type);
        assert(method->ret_type);
        if (!compatible_types(method->ret_type, ret_type)) {
            location_t loc_here = method->ret_expr->loc;
            const char *name = ret_type->name();
            const char *name2 = method->ret_type->name();
            const char *id = method->id;
            typecheck_error(method->ret_expr->loc, "The type: `", ret_type->name(),
                            "` of the return expression does not match the ",
                            "return type: `", method->ret_type->name(),
                            "` of method: `", method->id, "`");
        }
    }
    this->curr_method = NULL;

    // Free arena for labels
    deallocate(MEM::FUNC);
}

static Local *lookup_local(const char *id, Method *method, IdType *cls) {
    // Check current method's locals (vars and params).
    Local *local = method->locals.find(id);
    if (local) {
        return local;
    }
    // Check current class's fields.
    local = cls->fields.find(id);
    if (local) {
        return local;
    }
    // Check parent's fields (account for cyclic inheritance).
    IdType *runner = cls;
    while (runner->parent) {
        runner = cls->parent;
        local = runner->fields.find(id);
        if (local) {
            return local;
        }
        // Cyclic inheritance, we issue error elsewhere.
        if (runner == cls) break;
    }
    return NULL;
}

// Buf with memory arena MEM::FUNC
template<typename T> using FuncArr = Array<T, MEM::FUNC>;

static bool check_expr_list_against_method(FuncArr<Type*> expr_list, Method *method) {
    if (expr_list.len != method->param_len) {
        return false;
    }

    size_t formal_param_counter = 0;
    for (Type *ety : expr_list) {
        Type *formal_type = method->locals[formal_param_counter]->type;
        if (!compatible_types(formal_type, ety)) {
            return false;
        }
        ++formal_param_counter;
    }
    assert(formal_param_counter == method->param_len);
    return true;
}

static bool deduce_method(FuncArr<Type*> expr_list, const char *method_id, IdType *cls, Type **ret_type) {
    IdType *runner = cls;
    do {
        Method *method = lookup_method(method_id, runner);
        if (method) {
            if (runner == cls) {  // Only first iteration
                *ret_type = method->ret_type;
            }
            if (check_expr_list_against_method(expr_list, method)) {
                return true;
            }
        }
        runner = runner->parent;
        // Cyclic inheritance, we issue error elsewhere.
        if (runner == cls) break;
    } while (runner);
    return false;
}


/// LLVM CODEGEN

void emit(const char *fmt, ...) {
    if (config.codegen) {
        va_list args;
        va_start(args, fmt);
        print_indentation();
        vprintf(fmt, args);
        va_end(args);
    }
}

static llvalue_t llvm_op_const(int op, int val1, int val2) {
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

static void print_llvalue(llvalue_t v, bool its_bool = false) {
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

static llvalue_t llvm_op(int op, llvalue_t res1, llvalue_t res2) {
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

static llvalue_t llvm_getelementptr(llvalue_t ptr, llvalue_t index) {
    // TODO: Uncomment
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

static llvalue_t llvm_load(llvalue_t ptr) {
    llvalue_t v;
    v.reg = gen_reg();
    v.kind = LLVALUE::REG;
    print_codegen_indentation();
    emit("%%%ld = load i32, i32* %%%ld, align 4\n", v.reg, ptr.reg);
    return v;
}

static llvalue_t not_llvalue(llvalue_t v) {
    assert(v.kind == LLVALUE::REG);
    long reg = gen_reg();
    print_codegen_indentation();
    emit("%%%ld = icmp eq i1 %%%ld, 0\n", reg, v.reg);
    v.reg = reg;
    return v;
}

static llvalue_t llvm_calloc(int sz) {
    llvalue_t v;
    v.kind = LLVALUE::REG;
    v.reg = gen_reg();
    print_codegen_indentation();
    emit("%%%ld = call noalias i8* @calloc(i64 1, i64 %d)\n", v.reg, sz);
    return v;
}

static void llvm_gen_lbl(llvm_label_t l) {
    assert(!l.generated);
    emit("%s:\n", l.lbl);
    l.generated = true;
    __expr_context.curr_lbl = l;
}

static void llvm_branch_cond(llvalue_t cond, llvm_label_t l1, llvm_label_t l2) {
    print_codegen_indentation();
    emit("br i1 ");
    print_llvalue(cond);
    emit(", label %s, label %s\n\n", l1.lbl, l2.lbl);
}

static void llvm_branch(llvm_label_t l) {
    print_codegen_indentation();
    emit("br %s\n\n", l.lbl);
}

static llvalue_t llvm_and_phi(llvm_label_t l1, llvalue_t v1, llvm_label_t l2) {
    llvalue_t v;
    v.kind = LLVALUE::REG;
    v.reg = gen_reg();
    // We only need boolean values
    // TODO: Uncomment
    //assert(v1.kind == LLVALUE::REG || (v1.val == 0 || v1.val == 1));
    print_codegen_indentation();
    emit("%%%ld = phi i1 [ false, %s ], [ ", v.reg, l1.lbl);
    print_llvalue(v1);
    emit(", %s]\n", l2.lbl);
    return v;
}

// Only used for EXPR::AND
static Type *typecheck_and_helper(bool is_correct, Expression *expr,
                                  MainTypeCheckVisitor *v, Type *boolty, Type *undefinedty) {
    Type *ty = expr->accept(v);
    if (ty != boolty) {
        typecheck_error(expr->loc, "Bad right operand for binary operator `&&`. Operand ",
                        "of boolean type was expected, found: `", ty->name(), "`");
        is_correct = false;
    }
    if (is_correct) {
        return boolty;
    }
    return undefinedty;
}


// IMPORTANT: DO NOT check if a type is undefined with equality test with
// type_table.undefined_type. Check a note above on a full explanation.
// A lot of types can have remained undefined (either because we never saw a definition
// or the definition was invalid) but are not pointing to
// type_table.undefined_type. type_table.undefined_type is only for denoting
// an undefined type in general (e.g. an expression has undefined type, return
// type_table.undefined_type as its type. The caller checks for equality
// with type_table.undefined_type to see if it was valid).
Type* MainTypeCheckVisitor::visit(Expression *expr) {
    LOG_SCOPE;
    assert(!expr->is_undefined());

    // Used in binary expression cases.
    BinaryExpression *be = (BinaryExpression *) expr;

    switch (expr->kind) {
    case EXPR::BOOL_LIT:
    {
        debug_print("MainTypeCheck::BoolExpression\n");
        __expr_context.llval.kind = LLVALUE::CONST;
        __expr_context.llval.val = expr->lit_val;
        return this->type_table.bool_type;
    } break;
    case EXPR::ID:
    {
        debug_print("MainTypeCheck::IdExpression: %s\n", expr->id);
        assert(this->curr_method);
        assert(this->curr_class);
        Local *local = lookup_local(expr->id, this->curr_method, this->curr_class);
        // TODO: If the id is not local (parameter or variable), it will not have
        // a register assigned. Add the functionality to check if it has one
        // and if not, load the value into a register from the `this` pointer
        // (register %0). This should be done according to where the id exists
        // in the class (or the parent class etc.)

        __expr_context.llval.kind = LLVALUE::REG;
        __expr_context.llval.reg = local->reg;
        if (!local) {
            typecheck_error(expr->loc, "In identifier expression, Identifier: `",
                            expr->id, "` does is not defined.");
            return this->type_table.undefined_type;
        } else if (!local->initialized) {
            // Only variables can remain uninitialized. Check
            // TypeDeclaration and MethodDeclaration in DeclarationVisitor.
            typecheck_error(expr->loc, "Variable: `",
                            expr->id, "` might have not been initialized.");
        }
        return local->type;
    } break;
    case EXPR::INT_LIT:
    {
        debug_print("MainTypeCheck::IntegerExpression: %d\n", expr->lit_val);
        __expr_context.llval.kind = LLVALUE::CONST;
        __expr_context.llval.val = expr->lit_val;
        return this->type_table.int_type;
    } break;
    case EXPR::THIS:
    {
        debug_print("MainTypeCheck::ThisExpression\n");
        // Assume that every function has as a first argument
        // (so, register %0) a pointer to the calling class.
        // Assume also that this pointer is typed properly,
        // so no need to bitcast.
        __expr_context.llval.kind = LLVALUE::REG;
        __expr_context.llval.reg = 0;
        assert(this->curr_class);
        return this->curr_class;
    } break;
    case EXPR::ALLOC: // TODO: Codegen
    {
        debug_print("MainTypeCheck::AllocationExpression\n");
        // Find the type of the Identifier.
        IdType *type = this->type_table.find(expr->id);
        if (!type || type->kind == TY::UNDEFINED) {
            typecheck_error(expr->loc, "In allocation expression, Identifier: `",
                            expr->id, "` does not denote a known type");
            return this->type_table.undefined_type;
        }
        if (!type->is_IdType()) {
            typecheck_error(expr->loc, "In allocation expression, Identifier: `",
                            expr->id, "` does not denote a user-defined type");
            return this->type_table.undefined_type;
        }
        return type;
    } break;
    case EXPR::ARR_ALLOC: // TODO: Codegen
    {
        debug_print("MainTypeCheck::ArrayAllocationExpression\n");
        assert(expr->e1);
        Type *index_type = expr->e1->accept(this);
        if (index_type != this->type_table.int_type) {
            typecheck_error(expr->loc, "In array allocation expression, ",
                            "the index expression must be of integer type.");
            return this->type_table.undefined_type;
        }
        return this->type_table.int_arr_type;
    } break;
    case EXPR::ARR_LEN:
    {
        debug_print("MainTypeCheck::LengthExpression\n");
        assert(expr->e1);
        Type *arr = expr->e1->accept(this);
        llvalue_t ptr = __expr_context.llval;
        if (arr != this->type_table.int_arr_type) {
            typecheck_error(expr->loc, "In array length expression, ",
                            "the dereferenced id must be of integer array type.");
            return this->type_table.undefined_type;
        }
        // Codegen
        assert(ptr.kind == LLVALUE::REG);
        // Just loading a 4-byte value from `the` ptr will give us the length
        // as the first 4 bytes of arrays are the length.
        __expr_context.llval = llvm_load(ptr);
        return this->type_table.int_type;
    } break;
    case EXPR::NOT:
    {
        debug_print("MainTypeCheck::NotExpression\n");
        assert(expr->e1);
        Type *ty = expr->e1->accept(this);
        llvalue_t res = __expr_context.llval;
        if (ty != this->type_table.bool_type) {
            typecheck_error(expr->loc, "Bad operand for unary operator `!`. Operand ",
                            "of boolean type was expected, found: `", ty->name(), "`");
            return this->type_table.undefined_type;
        }
        if (res.kind == LLVALUE::CONST) {
            __expr_context.llval.kind = LLVALUE::CONST;
            __expr_context.llval.val = !res.val;
        } else {
            __expr_context.llval = not_llvalue(res);
        }
        return this->type_table.bool_type;
    } break;

/*----------- BINARY EXPRESSIONS ----------------*/

    case EXPR::AND:
    {
        debug_print("MainTypeCheck::AndExpression\n");
        assert(be->e1);
        assert(be->e2);
        Type *ty1 = be->e1->accept(this);
        llvalue_t res = __expr_context.llval;
        bool is_correct = true;

        llvm_label_t origin_lbl = __expr_context.curr_lbl;

        if (ty1 != this->type_table.bool_type) {
            typecheck_error(expr->loc, "Bad left operand for binary operator `&&`. Operand ",
                            "of boolean type was expected, found: `", ty1->name(), "`");
            is_correct = false;
        }

        // TODO: Currently, we can do constant-folding on the left expression. If it
        // is constant, we will know when it returns and it won't have emitted anything.
        // If not, it will have emitted and again we will know.
        // _However_, we can't constant fold the right expression. This is because
        // if it is not, we have to print a branch _before_ we process the right expression.
        // Possible solutions:
        // 1) The current one: constant fold only the left plus check if the right
        //    one is a trivial constant expression that we can know if it is constant
        //    a priori. Those are the BOOL_LIT expressions.
        // 2) Provide the ability to codegen in a buffer. That way, we don't print, but
        //    we output to the buffer that we can then use accordingly when we know
        //    what kind of expr we have.
        // 3) Disable the `config.codegen` (so, disable printing) and process the expr,
        //    learn if it is constant and if it's not, re-process it. That may be
        //    faster than it seems (and faster than 2) ).

        // Note: Read carefully the following code, it's crafted subtly.

        bool do_branching = true;
        if (res.kind == LLVALUE::CONST && res.val == 0) {
            // Constant left `false` value, don't run (i.e. codegen) the right expression.
            // `res` remains and is the result of the left expr.
            // We still have to type-check the right expr.
            if (res.val == 0) {
                // Turn off the codegen (if it was on), we only need to typecheck.
                bool codegen = config.codegen;
                config.codegen = false;
                Type *ty = typecheck_and_helper(is_correct, be->e2, this,
                                                this->type_table.bool_type,
                                                this->type_table.undefined_type);
                // We're done, restore it.
                config.codegen = codegen;
                // Restore
                __expr_context.llval = res;
                return ty;
            } else {
                // The result of the expr will be what the right expr is
                // and will be generated with the code following. No need
                // to do branching, phi etc.
                do_branching = false;
            }
        }
        
        lbl_pair_t and_lbls;
        if (do_branching) {
            // TODO: Here we need something like "insert BB", "insert BB after"
            // like LDC has.
            and_lbls.construct("and");
            
            // To reach this point, certainly res1.kind == LLVALUE::REG.
            // TODO: Uncomment
            //assert(res1.kind == LLVALUE::REG);
            llvm_branch_cond(res, and_lbls.start, and_lbls.end);
            llvm_gen_lbl(and_lbls.start);
        }

        Type *ret_ty = typecheck_and_helper(is_correct, be->e2, this,
                                    this->type_table.bool_type,
                                    this->type_table.undefined_type);
        llvm_label_t save_curr_lbl = __expr_context.curr_lbl;
        
        if (do_branching) {
            llvm_branch(and_lbls.end);

            llvm_gen_lbl(and_lbls.end);
            __expr_context.llval = llvm_and_phi(origin_lbl,
                                                __expr_context.llval, save_curr_lbl);
        } /* else {
            `__expr_context.llval` has the value generated
            when type-checking to receive `ret_ty`.
        }
        */
        return ret_ty;
    } break;

    case EXPR::CMP:
    {
        debug_print("MainTypeCheck::CmpExpression\n");
        assert(be->e1);
        assert(be->e2);
        Type *ty1 = be->e1->accept(this);
        llvalue_t res1 = __expr_context.llval;
        Type *ty2 = be->e2->accept(this);
        llvalue_t res2 = __expr_context.llval;
        bool is_correct = true;

        if (ty1 != this->type_table.int_type) {
            typecheck_error(expr->loc, "Bad left operand for binary operator `<`. Operand ",
                            "of int type was expected, found: `", ty1->name(), "`");
            is_correct = false;
        }
        if (ty2 != this->type_table.int_type) {
            typecheck_error(expr->loc, "Bad right operand for binary operator `<`. Operand ",
                            "of int type was expected, found: `", ty2->name(), "`");
            is_correct = false;
        }
        if (is_correct) {
            // Codegen
            __expr_context.llval = llvm_op('<', res1, res2);
            return this->type_table.bool_type;
        }
        return this->type_table.undefined_type;
    } break;
    case EXPR::PLUS:
    case EXPR::MINUS:
    case EXPR::TIMES:
    {
        assert(be->e1);
        assert(be->e2);

        int op;
        if (be->kind == EXPR::PLUS) {
            op = '+';
            debug_print("MainTypeCheck::PlusExpression\n");
        } else if (be->kind == EXPR::MINUS) {
            op = '-';
            debug_print("MainTypeCheck::MinusExpression\n");
        } else {
            op = '*';
            debug_print("MainTypeCheck::TimesExpression\n");
        }

        Type *ty1 = be->e1->accept(this);
        llvalue_t res1 = __expr_context.llval;
        Type *ty2 = be->e2->accept(this);
        llvalue_t res2 = __expr_context.llval;
        bool is_correct = true;



        if (ty1 != this->type_table.int_type) {
            typecheck_error(expr->loc, "Bad left operand for binary operator `", (char)op ,
                            "`. Operand of int type was expected, found: `",
                            ty1->name(), "`");
            is_correct = false;
        }
        if (ty2 != this->type_table.int_type) {
            typecheck_error(expr->loc, "Bad right operand for binary operator `", (char)op ,
                            "`. Operand of int type was expected, found: `",
                            ty2->name(), "`");
            is_correct = false;
        }
        if (is_correct) {
            // Codegen
            __expr_context.llval = llvm_op(op, res1, res2);
            return this->type_table.int_type;
        }
        return this->type_table.undefined_type;
    } break;
    case EXPR::ARR_LOOK:
    {
        debug_print("MainTypeCheck::ArrayLookupExpression\n");
        assert(be->e1);
        assert(be->e2);
        Type *ty1 = be->e1->accept(this);
        llvalue_t ptr = __expr_context.llval;
        Type *ty2 = be->e2->accept(this);
        llvalue_t index = __expr_context.llval;
        bool is_correct = true;

        if (ty1 != this->type_table.int_arr_type) {
            typecheck_error(expr->loc, "Bad left operand for index operator `[]`. ",
                            "Operand of int array type was expected, found: `",
                            ty1->name(), "`");
            is_correct = false;
        }
        if (ty2 != this->type_table.int_type) {
            typecheck_error(expr->loc, "Bad index expression for index operator `[]`. ",
                            "Operand of int type was expected, found: `",
                            ty1->name(), "`");
            is_correct = false;
        }
        if (index.kind == LLVALUE::CONST && index.val < 0) {
            typecheck_error(expr->loc, "Index with constant value: ", index.val,
                            " is out of bounds (negative)");
            is_correct = false;
        }
        if (is_correct) {
            // Check if index < len

            // Note: The length is never constant, so we can't
            // fully constant-fold this even if the index is constant (
            // we only partially constant-fold this in that the constant
            // value is used inline).
            lbl_pair_t lbls_len_check;
            lbls_len_check.construct("bounds_len");
            llvalue_t len = llvm_load(ptr);
            llvalue_t cmp_res = llvm_op('<', index, len);
            // Mind the order of labels
            llvm_branch_cond(cmp_res, lbls_len_check.end, lbls_len_check.start);

            llvm_gen_lbl(lbls_len_check.start);
            // TODO: Do something better
            emit("    EXIT\n\n");
            llvm_gen_lbl(lbls_len_check.end);


            // You can't have a constant of pointer value.
            // TODO: Uncomment
            //assert(ptr.kind == LLVALUE::REG);
            llvalue_t el;
            if (index.kind == LLVALUE::CONST) {
                // If index is constant and negative, we check it above.
                // Take into consideration the 4 bytes of the length.
                index.val += 4;
            } else {
                lbl_pair_t lbls_neg_check;
                lbls_neg_check.construct("bounds_neg");
                llvalue_t cmp_res = llvm_op('<', index, {LLVALUE::CONST, 0});
                llvm_branch_cond(cmp_res, lbls_neg_check.start, lbls_neg_check.end);

                llvm_gen_lbl(lbls_neg_check.start);
                // TODO: Do something better
                emit("    EXIT\n\n");
                llvm_gen_lbl(lbls_neg_check.end);
                index = llvm_op('+', index, {LLVALUE::CONST, 4});
            }
            ptr = llvm_getelementptr(ptr, index);
            __expr_context.llval = llvm_load(ptr);
            return this->type_table.int_type;
        }
        return this->type_table.undefined_type;
    } break;
    case EXPR::MSG_SEND:
    {
        // TODO: -- Codegen --
        // One possible implementation is to create a FuncArr of llvalue_t and
        // generate the expressions (preferably in reverse order to follow
        // the usual calling conventions although I'm not sure whether llvm
        // handles that - probably not). Note that this buf should allocate
        // memory from a method-persistent memory arena (that currently is
        // FUNC).
        debug_print("MainTypeCheck::MessageSendExpression\n");
        assert(be->e1);
        IdType *type = (IdType*) be->e1->accept(this);
        assert(type);

        if (type->kind == TY::UNDEFINED) {
            typecheck_error(expr->loc, "In message send expression, Identifier: `",
                            expr->id, "` does not denote a known type");
            return this->type_table.undefined_type;
        }
        if (!type->is_IdType()) {
            typecheck_error(expr->loc, "Bad dereferenced operand for message",
                            "send operator `.`. ", "Operand of user-defined ",
                            "type was expected, found: `", type->name(), "`");
            return this->type_table.undefined_type;
        }

        FuncArr<Type*> expr_list_types(be->msd->expr_list.len);
        for (Expression *e : be->msd->expr_list) {
            expr_list_types.push(e->accept(this));
        }
        Type *ret_type = NULL;
        if (!deduce_method(expr_list_types, be->msd->id, type, &ret_type)) {
            typecheck_error(be->loc, "No matching method with id: `", be->msd->id, "`");
        }
        // Assume that the user wanted the first method in the list
        if (!ret_type) {
            return this->type_table.undefined_type;
        }
        return ret_type;
    } break;
    default: assert(0); return this->type_table.undefined_type;
    }
}

/* Statements
 */
// TODO: Global error counting to known whether there was error in statement.
void MainTypeCheckVisitor::visit(BlockStatement *block_stmt) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::BlockStatement\n");
    for (Statement *stmt : block_stmt->block) {
        stmt->accept(this);
    }
}

void MainTypeCheckVisitor::visit(AssignmentStatement *asgn_stmt) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::AssignmentStatement\n");
    assert(this->curr_method);
    assert(this->curr_class);
    Local *lhs = lookup_local(asgn_stmt->id, this->curr_method, this->curr_class);
    if (!lhs) {
        // Of course, with variable we mean also parameter and field.
        typecheck_error(asgn_stmt->loc, "In assignment statement, variable: `",
                        asgn_stmt->id, "` is not defined");
    }
    assert(asgn_stmt->rhs);
    Type *rhs_type = asgn_stmt->rhs->accept(this);
    if (!compatible_types(lhs->type, rhs_type)) {
        typecheck_error(asgn_stmt->loc, "Incompatible types: `",
                        rhs_type->name(), "` can't be converted to `",
                        lhs->type->name(), "`");
    }
    lhs->initialized = true;
}

void MainTypeCheckVisitor::visit(ArrayAssignmentStatement *arr_asgn_stmt) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::ArrayAssignmentStatement\n");
    assert(this->curr_method);
    assert(this->curr_class);
    Local *arr = lookup_local(arr_asgn_stmt->id, this->curr_method, this->curr_class);
    if (!arr) {
        typecheck_error(arr_asgn_stmt->loc, "In array assignment statement, array: `",
                        arr_asgn_stmt->id, "` is not defined");
    }
    assert(arr_asgn_stmt->index);
    Type *index_type = arr_asgn_stmt->index->accept(this);
    if (index_type != this->type_table.int_type) {
        typecheck_error(arr_asgn_stmt->loc, "In array assignment statement, the ",
                        "index expression must have `int` type but has: `",
                        index_type->name(), "`");
    }
    assert(arr_asgn_stmt->rhs);
    Type *rhs_type = arr_asgn_stmt->rhs->accept(this);
    if (rhs_type != this->type_table.int_type) {
        typecheck_error(arr_asgn_stmt->loc, "In array assignment statement, the ",
                        "right-hand-side expression must have `int` type but has: `",
                        rhs_type->name(), "`");
    }
    arr->initialized = true;
}

void MainTypeCheckVisitor::visit(IfStatement *if_stmt) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::IfStatement\n");
    Type *cond = if_stmt->cond->accept(this);
    if (cond != this->type_table.bool_type) {
        typecheck_error(if_stmt->loc, "In if statement, the ",
                        "condition expression must have `boolean` type but has: `",
                        cond->name(), "`");
    }
    assert(if_stmt->then);
    assert(if_stmt->else_);
    if_stmt->then->accept(this);
    if_stmt->else_->accept(this);
}

void MainTypeCheckVisitor::visit(WhileStatement *while_stmt) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::WhileStatement\n");
    Type *cond = while_stmt->cond->accept(this);
    if (cond != this->type_table.bool_type) {
        typecheck_error(while_stmt->loc, "In while statement, the ",
                        "condition expression must have `boolean` type but has: `",
                        cond->name(), "`");
    }
    assert(while_stmt->body);
    while_stmt->body->accept(this);
}

void MainTypeCheckVisitor::visit(PrintStatement *print_stmt) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::PrintStatement\n");
    // A print statement can have any type, we visit it only for
    // type-checking purposes.
    print_stmt->to_print->accept(this);
}

/* Types
 */
void Type::print() const {
    LOG_SCOPE;
    switch (this->kind) {
    case TY::UNDEFINED: {
        red_on();
        printf("Undefined Type (Error)\n");
        red_off();
    } break;
    case TY::INT: {
        printf("int");
    } break;
    case TY::ARR: {
        printf("int[]");
    } break;
    case TY::BOOL: {
        printf("boolean");
    } break;
    // Should be handled by virtual print()
    // in IdType.
    case TY::ID: assert(0);
    default: assert(0);
    }
}

Method::Method(MethodDeclaration *method_decl) {
    id = method_decl->id;
    size_t locals_size = method_decl->params.len + method_decl->vars.len;
    locals.reserve(locals_size);
    param_len = method_decl->params.len;
    stmts = method_decl->stmts;
    ret_expr = method_decl->ret;
    loc = method_decl->loc;
    overrides = false;
}

void Method::print() const {
    LOG_SCOPE;
    debug_print("Method name: %s\n", this->id);
}

void IdType::print() const {
    LOG_SCOPE;
    debug_print("IdType: %s\n", this->id);
}
