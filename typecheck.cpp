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
#include "llvm.h"
#include "str_intern.h"

extern config_t config;

/* Errors
 */
// TODO: Stop typechecking altogether after a threshold of errors.
static int num_global_typecheck_errors;
template <typename... Args> 
void typecheck_error_no_ln(location_t __loc, Args... args) {
    ++num_global_typecheck_errors;
    log(__loc);
    yellow_on();
    bold_on();
    log(" Semantic Error: ");
    bold_off();
    yellow_off();
    log(args...);
}

template <typename... Args> 
static void typecheck_error(location_t __loc, Args... args) {
    ++num_global_typecheck_errors;
    log(__loc);
    yellow_on();
    bold_on();
    log(" Semantic Error: ");
    bold_off();
    yellow_off();
    log(args...);
    printf("\n");
}
/* End of errors */

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
        printf("%s", param->type->name());
        if (i + 1 != param_len) {
            printf(", ");
        }
    }
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


void check_method_for_overrding(Method *method, IdType *type) {
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
}

void TypeTable::compute_and_print_offsets_for_type(IdType *type) {
    assert(type);
    if (type->state == STATE::RESOLVING) {
        // We have cyclic inheritance.
        typecheck_error_no_ln(type->loc, "Cyclic inheritance ",
                        "involving: ");
        // Iterate the inheritance tree and print the whole cycle.
        IdType *runner = type;
        while (runner->parent) {
            printf("%s -> ", runner->id);
            runner = runner->parent;
            if (runner == type) {
                // reached the start of the cycle again.
                break;
            }
        }
        printf("%s\n", type->id);
        return;
    } else if (type->state == STATE::RESOLVED) {
        // We have already processed this type, just return.
        return;
    }
    type->state = STATE::RESOLVING;
    IdType *parent = type->parent;
    size_t start_fields = 0;
    size_t start_methods = 0;
    if (parent) {
        compute_and_print_offsets_for_type(parent);
        start_fields = parent->fields_end;
        start_methods = parent->methods_end;
        // Inherits, so include the parent at the start of the type.
        emit("%%class.%s = type { %%class.%s", type->id, parent->id);
    } else {
        // The first parent in the inheritance tree - create
        // a vptr.
        emit("%%class.%s = type { i8 (...)**", type->id);
    }

    // Process methods first to know how many methods we have so we
    // can emit the respective virtual pointer.
    int num_methods = start_methods / 8;
    size_t running_offset = start_methods;
    for (Method *method: type->methods) {
        check_method_for_overrding(method, type);
        if (!method->overrides) {
            if (config.offsets) {
                printf("%s.%s: %zd\n", type->id, method->id, running_offset);
            }
            method->offset = running_offset;
            running_offset += 8;
            ++num_methods;
        }
    }
    size_t methods_size = running_offset - start_methods;

    // Generate offsets for fields and also emit the fields.
    running_offset = start_fields;
    for (Local *field : type->fields) {
        field->offset = running_offset;
        if (config.offsets) {
            printf("%s.%s: %zd\n", type->id, field->id, field->offset);
        }
        emit(", ");
        if (field->type == this->bool_type) {
            emit("i1");
            running_offset += 1;
        } else if (field->type == this->int_type) {
            emit("i32");
            running_offset += 4;
        } else if (field->type == this->int_arr_type) {
            emit("i32*");
            running_offset += 8;
        } else {
            IdType *idtype = field->type->is_IdType();
            assert(idtype);
            emit("%%class.%s*", idtype->id);
            running_offset += 8;
        }
    }
    emit(" }\n");

    size_t fields_size = running_offset - start_fields;

    type->fields_end = start_fields + fields_size;
    type->methods_end = start_methods + methods_size;
    type->state = STATE::RESOLVED;
}

void TypeTable::offset_computation() {
    for (IdType *type : type_table) {
        compute_and_print_offsets_for_type(type);
    }
    emit("\n");
}

bool typecheck(Goal *goal) {
    typecheck_init();
    // Pass 1
    TypeTable type_table = install_type_declarations(goal);
    if (config.log) {
        printf("\n");
    }
    // TODO: How able we are to continue?
    // Print types that could not be inserted. This can
    // happen if the types used are more than the type
    // declarations. See TypeTable.
    // TODO: The types in `could_not_be_inserted` might actually have a
    // declaration. Consider that if say we have space in the table for 2
    // type declarations. The declarations that _exist_ are for A and D.
    // In A, let's say we use A, B, C and D in that order. D will be inserted
    // last and it won't be in the type table. It will be in the
    // `could_not_be_inserted` because of lack of space. But D
    // actually has a declaration.
    for (IdType *type : type_table.could_not_be_inserted) {
        typecheck_error(type->loc, "Type `", type->id, "` has not been ",
                        "defined");
    }

    // Offset computation
    type_table.offset_computation();

    // Pass 2
    full_typecheck(goal, type_table);
    
    if (num_global_typecheck_errors == 0) {
        return true;
    }
    return false;
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
            field->kind = (int)LOCAL_KIND::FIELD;
            type->fields.insert(field->id, field);
            /// Codegen ///
            
            /// End of Codegen ///
        }
    }
    for (MethodDeclaration *md : type_decl->methods) {
        // Check redefinition
        // IMPORTANT:
        // - A method must not conflict with a method of the current class.
        // - A method must not conflict with a field of the current class.
        // - A method can conflict with a method of the parent class:
        //   -- If the methods have the same return type (but possibly different formal
        //      parameters), it's valid overriding.
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

void MainTypeCheckVisitor::visit(IdType *type) {
    LOG_SCOPE;
    assert(type->is_IdType());
    assert(type->is_defined());
    debug_print("MainTypeCheck::IdType %s\n", type->id);

    this->curr_class = type;
    for (Method *method : type->methods) {
        method->accept(this, type->id);
    }
    this->curr_class = NULL;
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

extern ExprContext __expr_context;

void MainTypeCheckVisitor::visit(Method *method, const char *class_name) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::Method %s\n", method->id);
    this->curr_method = method;

    cgen_start_method(method, class_name);

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
        llvalue_t ret_val = __expr_context.llval;
        if (!compatible_types(method->ret_type, ret_type)) {
            location_t loc_here = method->ret_expr->loc;
            const char *name = ret_type->name();
            const char *name2 = method->ret_type->name();
            const char *id = method->id;
            typecheck_error(method->ret_expr->loc, "The type: `", ret_type->name(),
                            "` of the return expression does not match the ",
                            "return type: `", method->ret_type->name(),
                            "` of method: `", method->id, "`");
        } else {
            llvm_ret(ret_type, ret_val);
        }
    }
    this->curr_method = NULL;

    cgen_end_method();

    // Free arena for objects of function lifetime
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

static Method *deduce_method(FuncArr<Type*> expr_list, const char *method_id, IdType *cls) {
    IdType *runner = cls;
    do {
        Method *method = lookup_method(method_id, runner);
        if (method) {
            if (check_expr_list_against_method(expr_list, method)) {
                return method;
            }
        }
        runner = runner->parent;
        // Cyclic inheritance, we issue error elsewhere.
        if (runner == cls) break;
    } while (runner);
    return NULL;
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
// TODO: Maybe pass a second argument which is the expected return type. This is
// because a lot of times, when we find an error, we don't want to introduce useless
// cascading errors by return e.g. undefined_type. But currently, we can't know
// what the caller expects so we can just return that.
Type* MainTypeCheckVisitor::visit(Expression *expr) {
    LOG_SCOPE;
    assert(!expr->is_undefined());

    // Used in binary expression cases.
    BinaryExpression *be = (BinaryExpression *) expr;

    switch (expr->kind) {
    case EXPR::BOOL_LIT:
    {
        debug_print("MainTypeCheck::BoolExpression\n");
        /// Codegen ///
        __expr_context.llval.kind = LLVALUE::CONST;
        __expr_context.llval.val = expr->lit_val;
        /// End of Codegen ///
        return this->type_table.bool_type;
    } break;
    case EXPR::ID:
    {
        debug_print("MainTypeCheck::IdExpression: %s\n", expr->id);
        assert(this->curr_method);
        assert(this->curr_class);
        Local *local = lookup_local(expr->id, this->curr_method, this->curr_class);

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

        /// Codegen ///

        // We assume that we have correct type-checking (although we might not).
        // Given that, the 3 categories are handled as follows:
        // Params:
        //    Params have an automatically-assigned register and we just we use that.
        // Fields:
        //    For fields, we just load from register %0, which we assume corresponds
        //    to the `this` implicit parameter. Normally, we should use `getelementptr`
        //    (correctly) but this is kind of complicated, because the
        //    id can be in an arbitrary depth in the inheritance tree.
        //    What is more, the current offsets would
        //    not work (because they're absolute offsets, like the pointer is char*,
        //    not like getelementptr's). So, we assume %0 is i8*, move to the right position
        //    with `getelementptr` and we just bitcast it.
        // Vars:
        //    For vars, if the type-checking is correct, the will have been initialized.
        //    And thus, they should have a register assigned to them.
        LOCAL_KIND kind = (LOCAL_KIND)local->kind;
        switch (kind) {
        case LOCAL_KIND::PARAM:
        case LOCAL_KIND::VAR:
        {
            // Just use its llvalue.
            __expr_context.llval = local->llval;
        } break;
        case LOCAL_KIND::FIELD:
        {
            llvalue_t ptr = cgen_get_field_ptr(local);
            llvalue_t value = llvm_load(local->type, ptr);
            __expr_context.llval = value;
        } break;
        default: printf("id: %s\n", local->id); assert(0);
        }
        /// End of Codegen ///
        return local->type;
    } break;
    case EXPR::INT_LIT:
    {
        debug_print("MainTypeCheck::IntegerExpression: %d\n", expr->lit_val);
        /// Codegen ///
        __expr_context.llval.kind = LLVALUE::CONST;
        __expr_context.llval.val = expr->lit_val;
        /// End of Codegen ///
        return this->type_table.int_type;
    } break;
    case EXPR::THIS:
    {
        debug_print("MainTypeCheck::ThisExpression\n");
        /// Codegen ///

        // Assume that every function has as a first argument
        // (so, register %0) a pointer to the calling class.
        // Assume also that this pointer is typed properly,
        // so no need to bitcast.
        __expr_context.llval.kind = LLVALUE::REG;
        __expr_context.llval.reg = 0;

        /// End of Codegen ///

        assert(this->curr_class);
        return this->curr_class;
    } break;
    case EXPR::ALLOC:
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
        /// Codegen ///
        __expr_context.llval = llvm_calloc(type, {LLVALUE::CONST, (int)type->sizeof_()});
        /// End of Codegen ///
        return type;
    } break;
    case EXPR::ARR_ALLOC:
    {
        debug_print("MainTypeCheck::ArrayAllocationExpression\n");
        assert(expr->e1);
        Type *index_type = expr->e1->accept(this);
        if (index_type != this->type_table.int_type) {
            typecheck_error(expr->loc, "In array allocation expression, ",
                            "the index expression must be of integer type.");
        }
        llvalue_t len = __expr_context.llval;
        // Check for negative size
        if (len.kind == LLVALUE::CONST && len.val < 0) {
            typecheck_error(expr->loc, "Length of array allocation with ",
                            "constant value: ", len.val, " is negative.");
        }
        // TODO: Do a check when the value is not const.
        /// Codegen ///
        constexpr int sizeof_int = 4;
        Type *int_arr_type = this->type_table.int_arr_type;
        if (len.kind == LLVALUE::CONST) {
            __expr_context.llval = llvm_calloc(int_arr_type, {LLVALUE::CONST,
                                                   (int)(len.val * sizeof_int)});
        } else {
            llvalue_t size = llvm_op('*', len, {LLVALUE::CONST, (int)4});
            __expr_context.llval = llvm_calloc(int_arr_type, size);
        }
        /// End of Codegen ///
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
        }
        /// Codegen ///

        assert(ptr.kind == LLVALUE::REG);
        // Just loading a 4-byte value from `the` ptr will give us the length
        // as the first 4 bytes of arrays are the length.
        __expr_context.llval = llvm_load(this->type_table.int_type, ptr);

        /// End of Codegen ///

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
        }
        /// Codegen ///

        if (res.kind == LLVALUE::CONST) {
            __expr_context.llval.kind = LLVALUE::CONST;
            __expr_context.llval.val = !res.val;
        } else {
            __expr_context.llval = not_llvalue(res);
        }
        /// End of Codegen ///

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
        if (res.kind == LLVALUE::CONST) {
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
            assert(res.kind == LLVALUE::REG);
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

        if (ty1 != this->type_table.int_type) {
            typecheck_error(expr->loc, "Bad left operand for binary operator `<`. Operand ",
                            "of int type was expected, found: `", ty1->name(), "`");
        }
        if (ty2 != this->type_table.int_type) {
            typecheck_error(expr->loc, "Bad right operand for binary operator `<`. Operand ",
                            "of int type was expected, found: `", ty2->name(), "`");
        }
        /// Codegen  ///
        __expr_context.llval = llvm_op('<', res1, res2);
        /// End of Codegen  ///
        return this->type_table.bool_type;
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

        if (ty1 != this->type_table.int_type) {
            typecheck_error(expr->loc, "Bad left operand for binary operator `", (char)op ,
                            "`. Operand of int type was expected, found: `",
                            ty1->name(), "`");
        }
        if (ty2 != this->type_table.int_type) {
            typecheck_error(expr->loc, "Bad right operand for binary operator `", (char)op ,
                            "`. Operand of int type was expected, found: `",
                            ty2->name(), "`");
        }
        /// Codegen ///
        __expr_context.llval = llvm_op(op, res1, res2);
        /// End of Codegen ///
        return this->type_table.int_type;
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

        if (ty1 != this->type_table.int_arr_type) {
            typecheck_error(expr->loc, "Bad left operand for index operator `[]`. ",
                            "Operand of int array type was expected, found: `",
                            ty1->name(), "`");
        }
        if (ty2 != this->type_table.int_type) {
            typecheck_error(expr->loc, "Bad index expression for index operator `[]`. ",
                            "Operand of int type was expected, found: `",
                            ty1->name(), "`");
        }
        // Check for negative index 
        if (index.kind == LLVALUE::CONST && index.val < 0) {
            typecheck_error(expr->loc, "Index with constant value: ", index.val,
                            " is out of bounds (negative)");
        }

        /// Codegen ///

        // Check if index < len
        // Note: The length is never constant, so we can't
        // fully constant-fold this even if the index is constant (
        // we only partially constant-fold this in that the constant
        // value is used inline).
        lbl_pair_t lbls_len_check;
        lbls_len_check.construct("bounds_len");
        llvalue_t len = llvm_load(this->type_table.int_type, ptr);
        llvalue_t cmp_res = llvm_op('<', index, len);
        // Mind the order of labels
        llvm_branch_cond(cmp_res, lbls_len_check.end, lbls_len_check.start);

        llvm_gen_lbl(lbls_len_check.start);
        // TODO: Do something better
        emit("    EXIT\n\n");
        llvm_gen_lbl(lbls_len_check.end);


        // You can't have a constant of pointer value.
        assert(ptr.kind == LLVALUE::REG);
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
            index = llvm_op('+', index, {LLVALUE::CONST, (int)4});
        }
        ptr = llvm_getelementptr_i32(ptr, index);
        __expr_context.llval = llvm_load(this->type_table.int_type, ptr);

        /// End of Codegen ///

        return this->type_table.int_type;
    } break;
    case EXPR::MSG_SEND:
    {
        debug_print("MainTypeCheck::MessageSendExpression\n");
        assert(be->e1);
        IdType *type = (IdType*) be->e1->accept(this);
        llvalue_t base_obj = __expr_context.llval;
        assert(type);
        assert(type->is_IdType());

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
        FuncArr<llvalue_t> args(be->msd->expr_list.len);
        for (Expression *e : be->msd->expr_list) {
            Type *type = e->accept(this);
            args.push(__expr_context.llval);
            expr_list_types.push(type);
        }
        Method *method = deduce_method(expr_list_types, be->msd->id, type);
        if (!method) {
            typecheck_error(be->loc, "No matching method with id: `", be->msd->id, "`");
            // TODO: Could we return something better here? Like the type of the first
            // method in the inheritance tree?
            return this->type_table.undefined_type;
        }
        Type *ret_type = method->ret_type;
        // Load the virtual method pointer.
        llvalue_t vmethod = cgen_get_virtual_method(type, base_obj, method);
        __expr_context.llval = llvm_call(ret_type, type, base_obj,
                                         vmethod, expr_list_types, args);
        return ret_type;
    } break;
    default: assert(0); return this->type_table.undefined_type;
    }
}

/* Statements
 */
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
    bool is_correct = true;
    if (!lhs) {
        // Of course, with variable we mean also parameter and field.
        typecheck_error(asgn_stmt->loc, "In assignment statement, variable: `",
                        asgn_stmt->id, "` is not defined");
        is_correct = false;
    }
    assert(asgn_stmt->rhs);
    Type *rhs_type = asgn_stmt->rhs->accept(this);
    if (!compatible_types(lhs->type, rhs_type)) {
        typecheck_error(asgn_stmt->loc, "Incompatible types: `",
                        rhs_type->name(), "` can't be converted to `",
                        lhs->type->name(), "`");
    }
    if (!is_correct) {
        return;
    }
    /// Codegen ///
    llvalue_t value = __expr_context.llval;
    // Bitcast the value to the type of the lhs
    if (lhs->type != rhs_type) {
        value = cgen_cast_value(value, rhs_type, lhs->type);
    }
    
    if (lhs->kind == (int)LOCAL_KIND::FIELD) {
        llvalue_t ptr = cgen_get_field_ptr(lhs);
        llvm_store(lhs->type, value, ptr);
    } else if (!lhs->initialized && value.kind != LLVALUE::CONST) {
        // Assume that if it's not initialized,
        // the current assigned `llval` is a pointer.
        // The pointer we got from `alloca`.
        llvalue_t ptr = lhs->llval;
        llvm_store(lhs->type, value, ptr);
        // Load it so we have it in a register.
        // TODO: Should we postpone this until we have an actual use?
        llvalue_t loaded = llvm_load(lhs->type, ptr);
        lhs->llval = loaded;
    } else {
        lhs->llval = value;
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


const char *Type::name() const {
    switch (kind) {
    case TY::UNDEFINED: return "Undefined Type (Error)";
    case TY::INT: return "int";
    case TY::ARR: return "int[]";
    case TY::BOOL: return "boolean";
    case TY::ID: return ((IdType *)this)->id; 
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
