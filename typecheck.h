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
    size_t cap;  // For debugging

    SerializedHashTable() { }

    inline bool insert(const char *id, T v) {
        if (len == cap) return false;
        // Insert into HT
        if (table.insert(id, v)) {
            // Insert into serialized data
            serialized[len] = v;
            len = len + 1;
            return true;
        }
        return false;
    }

    inline void reserve(size_t n) {
        cap = n;
        len = 0;
        serialized = (T *) allocate(cap * sizeof(T), MEM::TYPECHECK);
        assert(serialized);
        table.reserve(cap);
    }

    inline T find(const char *key) {
        return table.find(key);
    }

    inline T* begin() { return this->serialized; }
    inline T* end() { return &this->serialized[len]; }

    T& operator[](size_t i) { return serialized[i]; }
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
    Buf<IdType*> could_not_be_inserted;

    TypeTable() { }

    // Note: The type table is initialized with a static
    // size, that is the number of type declarations.
    // In the most common case, the number of type
    // declarations match the number of types used (or
    // the latter is less). However, there is the case
    // that the user uses an undefined type and then
    // it is possible that the hash table does not
    // have space. We insert into an auxiliary
    // buffer and issue a message in the end of Pass 1.
    // Check definition of insert.

    void insert(const char *id, IdType* v);

    void initialize(size_t n);

    inline IdType* find(const char *key) {
        return type_table.find(key);
    }

    inline IdType** begin() { return type_table.begin(); }
    inline IdType** end() { return type_table.end(); }


    void compute_and_print_offsets_for_type(IdType *type);
    void offset_computation();
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
struct BlockStatement;
struct AssignmentStatement;
struct ArrayAssignmentStatement;
struct IfStatement;
struct WhileStatement;
struct PrintStatement;

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
    const char *gen_id();
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

    // Statements
    void    visit(BlockStatement *block_stmt);
    void    visit(AssignmentStatement *asgn_stmt);
    void    visit(ArrayAssignmentStatement *arr_asgn_stmt);
    void    visit(IfStatement *if_stmt);
    void    visit(WhileStatement *while_stmt);
    void    visit(PrintStatement *print_stmt);
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

    virtual const char *name() const {
        switch (kind) {
        case TY::UNDEFINED: return "Undefined Type (Error)";
        case TY::INT: return "int";
        case TY::ARR: return "int[]";
        case TY::BOOL: return "boolean";
        default: assert(0);
        }
    }
};

// TODO: Check if `id` fields are actually ever used for Locals
// and Methods
struct Local  : public TypeCheckCustomAllocation {
    const char *id;
    Type *type;
    bool initialized;

    Local() : id(NULL), initialized(false) { }
    Local(const char *_id, Type *_type) : id(_id), type(_type), initialized(false) { }

};

using Var = Local;
using Field = Local;
using Param = Local;

struct MethodDeclaration;
struct LocalDeclaration;
struct Statement;
struct Expression;

struct Method : public TypeCheckCustomAllocation {
    const char *id;
    Type *ret_type;
    size_t param_len;  // To know where params
    // end (and vars start).
    SerializedHashTable<Local*> locals;
    bool overrides;  // if it overrides parent method
    
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

enum class STATE {
    UNRESOLVED,
    RESOLVING,
    RESOLVED
};

struct IdType : public Type {
    SerializedHashTable<Local*> fields;
    SerializedHashTable<Method*> methods;
    IdType *parent;
    const char *id;

    size_t fields_end, methods_end;
    STATE state;

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
        kind = TY::ID;
        fields.reserve(nfields);
        methods.reserve(nmethods);
    }

    void set_parent(IdType *_parent) {
        parent = _parent;
    }

    void print() const override;

    const char *name() const override {
        return this->id;
    }

    IdType *is_IdType() override { return this; };

    void accept(MainTypeCheckVisitor *v) {
        v->visit(this);
    }
};


/* Prototypes
 */
void typecheck(Goal goal);

#endif
