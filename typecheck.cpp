#include <assert.h>
#include <stdio.h>

#include "ast.h"
#include "alloc.h"
#include "debug_print.h"
#include "error.h"          // log()
                            // WARNING: error.h gives access to the global `loc`
#include "hash_table.h"


#include "str_intern.h"

/// Fill the type_table.
void install_type_declarations(Goal goal) {
    DeclarationVisitor decl_visitor(goal.type_decls.len);
    goal.accept(&decl_visitor);
    
    /* Use with test.java */
    // TODO: remove that
    HashTable<IdType*> type_table = decl_visitor.type_table;
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
}

void typecheck_init() {
    set_indent_char('*');
}


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
    case TYSPEC::INT: return new Type(TY::INT);
    case TYSPEC::ARR: return new Type(TY::ARR);
    case TYSPEC::BOOL: return new Type(TY::BOOL);
    case TYSPEC::ID: return this->id_to_type(tspec.id);
    default: assert(0);
    }
}

/* Declaration Visitor
 */
DeclarationVisitor::DeclarationVisitor(size_t ntype_decls) {
    // IMPORTANT: Before installing a type, the user
    // is responsible for checking if it exists.
    this->type_table.reserve(ntype_decls);
}

void DeclarationVisitor::visit(Goal *goal) {
    LOG_SCOPE;
    //debug_print("Goal\n");
    goal->main_class.accept(this);
    for (TypeDeclaration *type_decl : goal->type_decls) {
        type_decl->accept(this);
    }
}

void DeclarationVisitor::visit(MainClass *main_class) {
    LOG_SCOPE;
    //debug_print("MainClass: %s\n", main_class->id);
}

void DeclarationVisitor::visit(TypeDeclaration *type_decl) {
    LOG_SCOPE;
    assert(!type_decl->is_undefined());
    print_indentation();
    log(type_decl->loc, "TypeDeclaration: ", type_decl->id, "\n");
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

/* Misc
 */
Method::Method(MethodDeclaration *method_decl) {
    id = method_decl->id;
    locals.reserve(method_decl->params.len + method_decl->vars.len);
    vars = method_decl->vars;
    stmts = method_decl->stmts;
    ret_expr = method_decl->ret;
}

/* Types
 */
void Type::print() const {
    LOG_SCOPE;
    switch (this->kind) {
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

void Method::print() const {
    LOG_SCOPE;
    debug_print("Method name: %s\n", this->id);
}

void IdType::print() const {
    LOG_SCOPE;
    debug_print("IdType: %s\n", this->id);
}
