#include <assert.h>
#include <stdio.h>

#include "ast.h"
#include "debug_print.h"
#include "error.h"          // log()
                            // WARNING: error.h gives access to the global `loc`
#include "hash_table.h"


#include "str_intern.h"

// IMPORTANT: Before installing a type, the user
// is responsible for checking if it exists.
static HashTable<IdType*> type_table;

/// Fill the type_table.
void install_type_declarations(Goal goal) {
    type_table.reserve(goal.type_decls.len);
    DeclarationVisitor decl_visitor(type_table);
    goal.accept(&decl_visitor);
    
    /* Use with test.java */
    // TODO: remove that
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
}

void typecheck_init() {
    set_indent_char('*');
}

Type *typespec_to_type(Typespec tspec) {
    switch (tspec.kind) {
    case TYSPEC::UNDEFINED: assert(0);
    case TYSPEC::INT: return new Type(TY::INT);
    case TYSPEC::ARR: return new Type(TY::ARR);
    case TYSPEC::BOOL: return new Type(TY::BOOL);
    case TYSPEC::ID:
    {
        // Check if it already exists.
        IdType *type = type_table.find(tspec.id);
        if (type) {
            return type;
        }
        // Otherwise construct a new one.
        return new IdType(tspec.id);
    } break;
    default: assert(0);
    }
}

/* Declaration Visitor
 */
DeclarationVisitor::DeclarationVisitor(HashTable<IdType*> type_table) {
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
    IdType *type = type_table.find(type_decl->id);
    // If it exists and it's declared (i.e. we have processed
    // a type with the same `id`), then we have redeclaration error.
    if (type) {
        if (type->is_defined()) {
            typecheck_error(type_decl->loc, "Type with id: ", type_decl->id,
                            " has already been declared.");
            // TODO: Make undefined and return.
            return;
        } else {
            type->set_sizes(type_decl->vars.len, type_decl->methods.len);
        }
    } else {
        type = new IdType(type_decl->id, type_decl->vars.len, type_decl->methods.len);
    }
    for (LocalDeclaration *ld : type_decl->vars) {
        // Check redeclaration
        Field *field = type->fields.find(ld->id);
        if (field) {
            typecheck_error(ld->loc, "In class with id: ", type_decl->id,
                            ", redeclaration of field with id: ", field->id, ".");
            // TODO: Make undefined (the whole type) and return.
        } else {
            field = ld->accept(this);
            //field->type->print();
            type->fields.insert(field->id, field);
        }
    }
    for (MethodDeclaration *md : type_decl->methods) {
        Method *method = md->accept(this);
        //method->print();
        type->methods.insert(method->id, method);
    }
    type_table.insert(type->id, type);
}

Local *DeclarationVisitor::visit(LocalDeclaration *local_decl) {
    LOG_SCOPE;
    assert(!local_decl->is_undefined());
    print_indentation();
    log(local_decl->loc, "LocalDeclaration: ", local_decl->id, "\n");
    Type *type = typespec_to_type(local_decl->typespec);
    Local *local = new Local(local_decl->id, type);
    return local;
}

Method *DeclarationVisitor::visit(MethodDeclaration *method_decl) {
    LOG_SCOPE;
    print_indentation();
    log(method_decl->loc, "MethodDeclaration: ", method_decl->id, "\n");
    assert(!method_decl->is_undefined());
    Method *method = new Method(method_decl->id, method_decl->params.len);
    for (LocalDeclaration *ld : method_decl->params) {
        Param* param = ld->accept(this);
        param->type->print();
        method->params.insert(param->id, param);
    }
    return method;
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
