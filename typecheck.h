#ifndef TYPECHECK_H

#define TYPECHECK_H

#include "alloc.h"
#include "buf.h"
#include "common.h"
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
    IdType *id_to_type(const char *id, location_t loc);
    Type *typespec_to_type(Typespec tspec, location_t loc);
};

/* Pass 2 Visitor
 */
struct expr_result_t;

struct MainTypeCheckVisitor {
    TypeTable type_table;
    Method *curr_method;
    IdType *curr_class;

    MainTypeCheckVisitor(TypeTable ttable) :
        type_table(ttable), curr_method(NULL), curr_class(NULL) { }
    void    visit(Goal *g);
    void    visit(MainClass *main_class);
    void    visit(IdType *type);
    void    visit(Method *method, const char *class_name);

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
    location_t loc;
    TY kind;

    Type() : kind(TY::UNDEFINED) { }
    Type(TY _kind) : kind(_kind) { }

    bool is_defined() const { return kind != TY::UNDEFINED; }
    const char *name() const;
    IdType *is_IdType() {
        if (kind == TY::ID) {
            return (IdType *) this;
        }
        return NULL;
    };
};

enum class LLVALUE {
    UNDEFINED,
    CONST,
    REG,
};

struct llvalue_t {
    LLVALUE kind;
    union {
        long reg;
        int val;
    };

    llvalue_t() { }


    llvalue_t(bool _val) {
        kind = LLVALUE::CONST;
        val = (int) _val;
    }

    // TODO - IMPORTANT: This is a very bad design because the user
    // has to specifically cast to `int` or `long`
    // to disambiguate.

    llvalue_t(LLVALUE _kind, int _val) {
        assert(_kind == LLVALUE::CONST);
        kind = _kind;
        val = _val;
    }
    llvalue_t(LLVALUE _kind, long _reg) {
        assert(_kind == LLVALUE::REG);
        kind = _kind;
        reg = _reg;
    }
};

enum class LOCAL_KIND {
  UNDEFINED,
  PARAM,
  FIELD,
  VAR
};

// TODO: Check if `id` fields are actually ever used for Locals
// and Methods
// TODO: We could save the size of arrays if it is known at compile-time
// (i.e. it is constant) and then if we index it with a constant index
// and it is out of bounds, we can issue an error at compile-time. That
// requires a little bit of thinking because we need 4 more bytes in each
// local. Those are not very much, but they will be wasted for all other
// locals. Also, this scenario is not very possible.
struct Local  : public TypeCheckCustomAllocation {
    const char *id;
    Type *type;
    union {
        // TODO:
        // In practice, this wastes bytes. That is, we only need
        // either the `reg` or the `val` from an llvalue and know
        // either if it is const or not. Note that the latter is
        // 1 bit of info, but we're getting from `kind`. And because
        // it is an `enum`, it will take 8 bytes. We assume that
        // the locals won't be very much, so for now, we can
        // live with it.
        llvalue_t llval;
        // Offset for fields.
        size_t offset;
    };
    size_t index;
    // We have to use an `int` instead of LOCAL_KIND to suppress
    // stupid warning.
    int kind : 3;
    bool initialized : 1;

    Local() : id(NULL), initialized(false) { }
    Local(const char *_id, Type *_type) : id(_id), type(_type), initialized(false) { }

    size_t offsetof_() const {
        // +8 for the virtual pointer.
        return offset + 8;
    }
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
    // Offset in the virtual table
    size_t offset;
    location_t loc;
    bool overrides;  // if it overrides parent method
    
    // Copied from the MethodDeclaration
    Buf<Statement*> stmts;
    Expression *ret_expr;

    Method() = delete;
    Method(MethodDeclaration *method_decl);

    void accept(MainTypeCheckVisitor *v, const char *class_name) {
        v->visit(this, class_name);
    }

    size_t offsetof_() const {
        return offset;
    }
};

enum class STATE {
    UNRESOLVED,
    RESOLVING,
    RESOLVED
};

struct IdType : public Type {
    const char *id;
    SerializedHashTable<Local*> fields;
    SerializedHashTable<Method*> methods;
    IdType *parent;

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

    void accept(MainTypeCheckVisitor *v) {
        v->visit(this);
    }

    size_t sizeof_() const {
        // +8 for the virtual pointer
        return fields_end + 8;
    }
};

/* Prototypes
 */
bool typecheck(Goal *goal);

#endif
