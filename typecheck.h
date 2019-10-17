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

struct Type {
    TY kind;

    Type(TY _kind) : kind(_kind) { }

    virtual void print() const;
};

struct Local {
    const char *id;
    Type *type;

    Local(const char *_id, Type *_type) : id(_id), type(_type) { }

};

using Field = Local;
using Param = Local;

struct Method {
    const char *id;
    Type *ret_type;
    HashTable<Local*> params;
};

struct IdType : public Type {
    const char *id;
    HashTable<Local*> fields;
    HashTable<Method*> methods;

    IdType(const char *_id) : Type(TY::ID), id(_id) { }

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
    void visit(Goal *g);
    void visit(MainClass *main_class);
    void visit(TypeDeclaration *type_decl);
    Local *visit(LocalDeclaration *local_decl);
    void visit(MethodDeclaration *method_decl);
};

/* Prototypes
 */
void install_type_declarations(Goal goal);
void typecheck_init();

#endif
