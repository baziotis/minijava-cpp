#include <assert.h>
#include <stdio.h>

#include "ast.h"
#include "alloc.h"
#include "debug_print.h"
#include "error.h"          // log()
                            // WARNING: error.h gives access to the global `loc`
#include "hash_table.h"
#include "str_intern.h"

void typecheck_init() {
    set_indent_char('*');
}

/// Fill the type_table.
TypeTable install_type_declarations(Goal *goal) {
    DeclarationVisitor decl_visitor(goal->type_decls.len);
    goal->accept(&decl_visitor);
    
    /* Use with test.java */
    // TODO: remove that
    TypeTable type_table = decl_visitor.type_table;
    Type *type = type_table.find(str_intern("A"));
    assert(type);
    IdType *id_type = type->is_IdType();
    assert(id_type);
    Field *field = id_type->fields.find(str_intern("b"));
    assert(field);
    Method *method = id_type->methods.find(str_intern("test"));
    assert(method);

    assert(id_type->fields.find(str_intern("c")) == NULL);
    assert(id_type->methods.find(str_intern("other")) == NULL);

    Type *D_type = type_table.find(str_intern("D"));
    assert(D_type->is_IdType());
    IdType *C_type = type_table.find(str_intern("C"));
    assert(C_type->is_IdType());
    Field *d2_field = C_type->fields.find(str_intern("d2"));
    assert(d2_field->type == D_type);

    // Test inheritance (A is supposed to inherit from B)
    IdType *B_type = type_table.find(str_intern("B"));
    IdType *A_type = type_table.find(str_intern("A"));
    assert(A_type);
    assert(B_type);
    assert(A_type->is_IdType());
    assert(B_type->is_IdType());
    assert(A_type->parent);
    assert(A_type->parent == B_type);


    // Free memory that was used for allocating objects
    // for parsing that doenot persist in pass 2 of type-checking.
    deallocate(MEM::PARSE_TEMP);

    return decl_visitor.type_table;
}

void full_typecheck(Goal *goal, TypeTable type_table) {
    MainTypeCheckVisitor main_visitor(type_table);
    goal->accept(&main_visitor);
}

void typecheck(Goal goal) {
    typecheck_init();
    // Pass 1
    TypeTable type_table = install_type_declarations(&goal);
    printf("\n\n\n\n");
    // Pass 2
    full_typecheck(&goal, type_table);
}


/* Declaration Visitor
 */
IdType* DeclarationVisitor::id_to_type(const char *id) {
    // Check if it already exists.
    IdType *type = this->type_table.find(id);
    if (type) {
        return type;
    }
    // Otherwise construct a new one and
    // save it in the table.
    type = new IdType(id);
    this->type_table.insert(type->id, type);
    return type;
}

// This is member of the visitor so that we have
// access to the type table.
Type* DeclarationVisitor::typespec_to_type(Typespec tspec) {
    switch (tspec.kind) {
    case TYSPEC::UNDEFINED: assert(0);
    case TYSPEC::INT: return type_table.int_type;
    case TYSPEC::ARR: return type_table.int_arr_type;
    case TYSPEC::BOOL: return type_table.bool_type;
    case TYSPEC::ID: return this->id_to_type(tspec.id);
    default: assert(0);
    }
}

void TypeTable::initialize(size_t n) {
    type_table.reserve(n);
    undefined_type = new Type(TY::UNDEFINED);
    bool_type = new Type(TY::BOOL);
    int_type = new Type(TY::INT);
    int_arr_type = new Type(TY::ARR);
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
    log(type_decl->loc, "Pass1::TypeDeclaration: ", type_decl->id, "\n");
    IdType *type = this->type_table.find(type_decl->id);
    // If it exists and it's declared (i.e. we have processed
    // a type with the same `id`), then we have redeclaration error.
    if (type) {
        if (type->is_defined()) {
            typecheck_error(type_decl->loc, "Type with id: `", type_decl->id,
                            "` has already been declared");
            // TODO: Make undefined and return.
            return;
        } else {
            type->set_sizes(type_decl->vars.len, type_decl->methods.len);
        }
    } else {
        type = new IdType(type_decl->id, type_decl->vars.len, type_decl->methods.len);
        // TODO: insert it as undefined if it's not correct.
        this->type_table.insert(type->id, type);
    }
    // Handle inheritance
    if (type_decl->extends) {
        IdType *parent = this->id_to_type(type_decl->extends);
        type->set_parent(parent);
    }
    for (LocalDeclaration *ld : type_decl->vars) {
        // Check redeclaration
        Field *field = type->fields.find(ld->id);
        if (field) {
            typecheck_error(ld->loc, "In class with id: `", type_decl->id,
                            "`, redeclaration of field with id: `", field->id, "`");
            // TODO: Make undefined (the whole type) and return.
        } else {
            field = ld->accept(this);
            //field->type->print();
            type->fields.insert(field->id, field);
        }
    }
    for (MethodDeclaration *md : type_decl->methods) {
        // Check redeclaration
        Method *method = type->methods.find(md->id);
        if (method) {
            typecheck_error(md->loc, "In class with id: `", type_decl->id,
                            "`, redeclaration of method with id: `", method->id, "`");
            // TODO: Make undefined (the whole type) and return.
        } else {
            method = md->accept(this);
            //method->print();
            type->methods.insert(method->id, method);
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
    log(local_decl->loc, "LocalDeclaration: ", local_decl->id, "\n");
    Type *type = typespec_to_type(local_decl->typespec);
    Local *local = new Local(local_decl->id, type);
    return local;
}

Method *DeclarationVisitor::visit(MethodDeclaration *method_decl) {
    // Note: It's responsibility of the one who calls this visit
    // to have assured that a method with the same id does not exist.
    LOG_SCOPE;
    assert(!method_decl->is_undefined());
    print_indentation();
    log(method_decl->loc, "MethodDeclaration: ", method_decl->id, "\n");
    Method *method = new Method(method_decl);
    method->ret_type = this->typespec_to_type(method_decl->typespec);
    for (LocalDeclaration *par : method_decl->params) {
        if (method->locals.find(par->id)) {
            typecheck_error(par->loc, "Parameter `", par->id, "` is already defined",
                            " in method `", method_decl->id, "`");
        } else {
            Param* param = par->accept(this);
            param->type->print();
            method->locals.insert(param->id, param);
        }
    }
    for (LocalDeclaration *var : method_decl->vars) {
        if (method->locals.find(var->id)) {
            typecheck_error(var->loc, "Variable `", var->id, "` is already defined",
                            " in method `", method_decl->id, "`");
        } else {
            Var *v = var->accept(this);
            v->type->print();
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
        type->accept(this);
    }
}

void MainTypeCheckVisitor::visit(MainClass *main_class) {
    LOG_SCOPE;
    //debug_print("MainTypeCheck::MainClass\n");
}

void MainTypeCheckVisitor::visit(IdType *type) {
    LOG_SCOPE;
    assert(type->is_IdType());
    debug_print("MainTypeCheck::IdType %s\n", type->id);

    this->curr_class = type;
    for (Method *method : type->methods) {
        method->accept(this);
    }
    this->curr_class = NULL;
}

void MainTypeCheckVisitor::visit(Method *method) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::Method %s\n", method->id);
    this->curr_method = method;
    // An undefined return expression may actually end up here.
    // Check parse.cpp
    if (!method->ret_expr->is_undefined()) {
        Type *ret_type = method->ret_expr->accept(this);
        ret_type->print();
    }
    this->curr_method = NULL;
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
    debug_print("MainTypeCheck::Expression\n");
    switch (expr->kind) {
    case EXPR::BOOL_LIT:
    {
        return this->type_table.bool_type;
    } break;
    case EXPR::ID:
    {
        // Check current method's locals (vars and params).
        Method *method = this->curr_method;
        Local *local = method->locals.find(expr->id);
        if (local) {
            return local->type;
        }
        // Check current class's fields.
        // TODO: Encapsulate the lookup of the field in a class
        // and its parents?
        IdType *cls = this->curr_class;
        local = cls->fields.find(expr->id);
        if (local) {
            return local->type;
        }
        // Check parent's fields.
        while (cls->parent) {
            cls = cls->parent;
            local = cls->fields.find(expr->id);
            if (local) {
                return local->type;
            }
        }
        return this->type_table.undefined_type;
    } break;
    case EXPR::INT_LIT:
    {
        return this->type_table.int_type;
    } break;
    case EXPR::THIS:
    {
        return this->curr_class;
    } break;
    case EXPR::ALLOC:
    {
        // Find the type of the Identifier.
        IdType *type = this->type_table.find(expr->id);
        if (!type) {
            typecheck_error(expr->loc, "In allocation expression, Identifier: `",
                            expr->id, "` does not denote a known type");
            return this->type_table.undefined_type;
        }
        if (type->kind == TY::UNDEFINED) {
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
    case EXPR::ARR_ALLOC:
    {
        Type *index_type = expr->e1->accept(this);
        index_type->print();
        if (index_type != this->type_table.int_type) {
            typecheck_error(expr->loc, "In array allocation expression, ",
                            "the index expression must be of integer type.");
            return this->type_table.undefined_type;
        }
        return this->type_table.int_arr_type;
    } break;
    case EXPR::ARR_LEN:
    {
        Type *arr = expr->e1->accept(this);
        if (arr != this->type_table.int_arr_type) {
            typecheck_error(expr->loc, "In array length expression, ",
                            "the dereferenced id must be of integer array type.");
            return this->type_table.undefined_type;
        }
        return this->type_table.int_type;
    } break;
    case EXPR::NOT:
    {
        Type *ty = expr->e1->accept(this);
        if (ty != this->type_table.bool_type) {
            typecheck_error(expr->loc, "Bad operand for unary operator `!`. Operand ",
                            "of boolean type was expected");
            // TODO: Found what? It requires some changes to the log in error.cpp
            return this->type_table.undefined_type;
        }
        return this->type_table.bool_type;
    } break;
    case EXPR::UNDEFINED:
    {
        return this->type_table.undefined_type;
    } break;
    default: assert(0); return this->type_table.undefined_type;
    }
}

/* Types
 */
void Type::print() const {
    LOG_SCOPE;
    switch (this->kind) {
    case TY::UNDEFINED: {
        red_on();
        debug_print("Undefined Type (Error)\n");
        red_off();
    } break;
    case TY::INT: {
        debug_print("Int\n");
    } break;
    case TY::ARR: {
        debug_print("Array\n");
    } break;
    case TY::BOOL: {
        debug_print("Boolean\n");
    } break;
    // Should be handled by virtual print()
    // in IdType.
    case TY::ID: assert(0);
    default: assert(0);
    }
}

Method::Method(MethodDeclaration *method_decl) {
    id = method_decl->id;
    locals.reserve(method_decl->params.len + method_decl->vars.len);
    stmts = method_decl->stmts;
    ret_expr = method_decl->ret;
}

void Method::print() const {
    LOG_SCOPE;
    debug_print("Method name: %s\n", this->id);
}

void IdType::print() const {
    LOG_SCOPE;
    debug_print("IdType: %s\n", this->id);
}
