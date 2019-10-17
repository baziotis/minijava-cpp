#include <assert.h>
#include <stdio.h>

#include "ast.h"
#include "debug_print.h"
#include "error.h"          // log()
                            // WARNING: error.h gives access to the global `loc`
#include "hash_table.h"

static HashTable<IdType> type_table;

/// Fill the type_table.
void install_type_declarations(Goal goal) {
    DeclarationVisitor decl_visitor(type_table);
    goal.accept(&decl_visitor);
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
    case TYSPEC::ID: return new IdType(TY::ID, tspec.id);
    default: assert(0);
    }
}

/* Declaration Visitor
 */
DeclarationVisitor::DeclarationVisitor(HashTable<IdType> type_table) {
    printf("INITIALIZED\n");
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
    for (LocalDeclaration *ld : type_decl->vars) {
        Field* field = ld->accept(this);
        field->type->print();
    }
    for (MethodDeclaration *md : type_decl->methods) {
        md->accept(this);
    }
}

Local *DeclarationVisitor::visit(LocalDeclaration *local_decl) {
    LOG_SCOPE;
    print_indentation();
    assert(!local_decl->is_undefined());
    log(local_decl->loc, "LocalDeclaration: ", local_decl->id, "\n");
    Type *type = typespec_to_type(local_decl->typespec);
    Local *local = new Local(local_decl->id, type);
    return local;
}

void DeclarationVisitor::visit(MethodDeclaration *method_decl) {
    LOG_SCOPE;
    print_indentation();
    log(method_decl->loc, "MethodDeclaration: ", method_decl->id, "\n");
    assert(!method_decl->is_undefined());
    for (LocalDeclaration *ld : method_decl->params) {
        ld->accept(this);
    }
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

void IdType::print() const {
    LOG_SCOPE;
    debug_print("IdType: %s\n", this->id);
}
