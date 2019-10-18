#ifndef TYPECHECK_H
#define TYPECHECK_H

#include "alloc.h"
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

template<typename T> using HashTable = __HashTable<T, MEM::TYPECHECK>;
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

using Var = Local;
using Field = Local;
using Param = Local;

class MethodDeclaration;
class LocalDeclaration;
class Statement;
class Expression;

struct Method {
    const char *id;
    Type *ret_type;
    HashTable<Local*> locals;
    
    // Copied from the MethodDeclaration
    Buf<LocalDeclaration*> vars;
    Buf<Statement*> stmts;
    Expression *ret_expr;

    Method() = delete;
    Method(MethodDeclaration *method_decl);

    void print() const;
};

struct IdType : public Type {
    HashTable<Local*> fields;
    HashTable<Method*> methods;
    IdType *parent;
    const char *id;

    IdType() : id(NULL), parent(NULL) { }
    // For type we've not processed yet (so, we know
    // only its id). But, clarify that it's undefined.
    IdType(const char *_id) : Type(TY::UNDEFINED), id(_id), parent(NULL) { }
    // For type we're currently processing.
    IdType(const char *_id, size_t nfields, size_t nmethods) : 
        Type(TY::ID), id(_id), parent(NULL)
    {
        this->set_sizes(nfields, nmethods);
    }

    void set_sizes(size_t nfields, size_t nmethods) {
        fields.reserve(nfields);
        methods.reserve(nmethods);
    }

    void set_parent(IdType *_parent) {
        parent = _parent;
    }

    IdType *is_IdType() override { return this; };

    void print() const override;
};

/* Declaration Visitor
 */
class Goal;
class MainClass;
class TypeDeclaration;
class Typespec;

struct DeclarationVisitor {
    HashTable<IdType*> type_table;

    DeclarationVisitor(size_t ntype_decls);
    void    visit(Goal *g);
    void    visit(MainClass *main_class);
    void    visit(TypeDeclaration *type_decl);
    Local*  visit(LocalDeclaration *local_decl);
    Method* visit(MethodDeclaration *method_decl);

    // Helper functions
    IdType *id_to_type(const char *id);
    Type *typespec_to_type(Typespec tspec);
};

/* Prototypes
 */
void install_type_declarations(Goal goal);
void typecheck_init();

#endif
