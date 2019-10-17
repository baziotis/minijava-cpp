#ifndef TYPECHECK_H
#define TYPECHECK_H

#include "buf.h"
#include "hash_table.h"

/* Types
*/
struct TypeCheckCustomAllocation {
    static void *operator new(size_t size) {
        return allocate_zero(size, MEM::TYPECHECK);
    }
};

enum class TY {
    UNDEFINED,
    INT,
    ARR,
    BOOL,
    ID,
};

// TODO: It may be a good idea to add a location_t
// field (in all the structs below?) to redirect the user when
// type errors occur for a type.
// For example:
// - Type with id: X was declared again in location: Y
// - Type with id: X does not have field Y, check declaration in: Z
// - In class X, the field with id: Y has been redeclared in Z

struct IdType;

struct Type {
    TY kind;

    Type() : kind(TY::UNDEFINED) { }
    Type(TY _kind) : kind(_kind) { }

    bool is_defined() const { return kind != TY::UNDEFINED; }

    virtual IdType *is_IdType() { return NULL; };

    virtual void print() const;
};

struct Local {
    const char *id;
    Type *type;

    Local() : id(NULL) { }
    Local(const char *_id, Type *_type) : id(_id), type(_type) { }

};

using Field = Local;
using Param = Local;

struct Method {
    const char *id;
    Type *ret_type;
    HashTable<Local*> params;

    Method() : id(NULL) {}
    Method(const char *_id, size_t nparams) : id(_id) {
        params.reserve(nparams);
    }

    void print() const;
};

struct IdType : public Type {
    HashTable<Local*> fields;
    HashTable<Method*> methods;
    const char *id;

    IdType() : id(NULL) { }
    // For type we've not processed yet (so, we know
    // only its id). But, clarify that it's undefined.
    IdType(const char *_id) : Type(TY::UNDEFINED), id(_id) { }
    // For type we're currently processing.
    IdType(const char *_id, size_t nfields, size_t nmethods) : 
        Type(TY::ID), id(_id)
    {
        this->set_sizes(nfields, nmethods);
    }

    void set_sizes(size_t nfields, size_t nmethods) {
        fields.reserve(nfields);
        methods.reserve(nmethods);
    }

    IdType *is_IdType() override { return this; };

    void print() const override;
};

/* Declaration Visitor
 */
class Goal;
class MainClass;
class TypeDeclaration;
class MethodDeclaration;
class LocalDeclaration;

struct DeclarationVisitor {
    DeclarationVisitor(HashTable<IdType*> type_table);
    void    visit(Goal *g);
    void    visit(MainClass *main_class);
    void    visit(TypeDeclaration *type_decl);
    Local*  visit(LocalDeclaration *local_decl);
    Method* visit(MethodDeclaration *method_decl);
};

/* Prototypes
 */
void install_type_declarations(Goal goal);
void typecheck_init();

#endif
