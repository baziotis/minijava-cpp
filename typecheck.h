#ifndef TYPECHECK_H
#define TYPECHECK_H

#include "alloc.h"
#include "buf.h"
#include "hash_table.h"

template<typename T> using HashTable = __HashTable<T, MEM::TYPECHECK>;
// Sort of like combining a Buf and a HashTable
template<typename T>
struct SerializedHashTable {
    HashTable<T> table;
    T *serialized;
    size_t len;

    SerializedHashTable() { }

    inline void insert(const char *id, T v) {
        // Insert into serialized data
        serialized[len] = v;
        len = len + 1;
        // Insert into HT
        table.insert(id, v);
    }

    inline void reserve(size_t n) {
        len = 0;
        serialized = (T *) allocate(n * sizeof(T), MEM::TYPECHECK);
        table.reserve(n);
    }

    inline T find(const char *key) {
        return table.find(key);
    }

    inline T* begin() { return this->serialized; }
    inline T* end() { return &this->serialized[len]; }
};

struct IdType;
struct Type;

// A SerializedHashTable with just hardcoded the primitive types.
struct TypeTable {
    Type *undefined_type;
    Type *bool_type;
    Type *int_type;
    Type *int_arr_type;
    SerializedHashTable<IdType*> type_table;

    TypeTable() { }

    inline void insert(const char *id, IdType* v) {
        type_table.insert(id, v);
    }

    void initialize(size_t n);

    inline IdType* find(const char *key) {
        return type_table.find(key);
    }

    inline IdType** begin() { return type_table.begin(); }
    inline IdType** end() { return type_table.end(); }
};

/* Pass 1 Visitor
 */
struct Goal;
struct MainClass;
struct TypeDeclaration;
struct LocalDeclaration;
struct MethodDeclaration;
struct Typespec;
struct Expression;
struct BinaryExpression;

struct Local;
struct Method;

struct DeclarationVisitor {
    TypeTable type_table;

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

/* Pass 2 Visitor
 */
struct MainTypeCheckVisitor {
    TypeTable type_table;
    Method *curr_method;
    IdType *curr_class;

    MainTypeCheckVisitor(TypeTable ttable) :
        type_table(ttable), curr_method(NULL), curr_class(NULL) { }
    void    visit(Goal *g);
    void    visit(MainClass *main_class);
    void    visit(IdType *type);
    void    visit(Method *method);

    Type*   visit(Expression *expr);
    Type*   visit(BinaryExpression *bin_expr);
};

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

struct Type : public TypeCheckCustomAllocation {
    TY kind;

    Type() : kind(TY::UNDEFINED) { }
    Type(TY _kind) : kind(_kind) { }

    bool is_defined() const { return kind != TY::UNDEFINED; }

    virtual IdType *is_IdType() { return NULL; };

    virtual void print() const;
};

struct Local  : public TypeCheckCustomAllocation {
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

struct Method : public TypeCheckCustomAllocation {
    const char *id;
    Type *ret_type;
    HashTable<Local*> locals;
    
    // Copied from the MethodDeclaration
    Buf<Statement*> stmts;
    Expression *ret_expr;

    Method() = delete;
    Method(MethodDeclaration *method_decl);

    void print() const;

    void accept(MainTypeCheckVisitor *v) {
        v->visit(this);
    }
};

struct IdType : public Type {
    HashTable<Local*> fields;
    SerializedHashTable<Method*> methods;
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

    void print() const override;

    IdType *is_IdType() override { return this; };

    void accept(MainTypeCheckVisitor *v) {
        v->visit(this);
    }
};


/* Prototypes
 */
void typecheck(Goal goal);

#endif
