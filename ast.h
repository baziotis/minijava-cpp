#ifndef AST_H
#define AST_H

#include "alloc.h"    // MEM, allocate_zero()
#include "buf.h"
#include "typecheck.h"
#include "common.h"

// TODO: Should the `Buf`s have the actual type and not a pointer to it?
// TODO: Separate memory allocation to arenas that can be thrown after
//       pass 1 of typechecking and those that can't. For example, in
//       a MethodDeclaration, we can throw the `params` but not `vars`,
//       `smts` and `ret`.
//       A TypeDeclaration can completely be thrown away, except for its
//       `methods`. 

// Pre-declarations
struct Expression;

struct ParsingPersistentAllocation {
    // TODO: We could have a memory arena per class
    // but that would require some changes to the allocator.
    static void *operator new(size_t size) {
        return allocate_zero(size, MEM::PARSE_PERSIST_TYPECHECK_PASS2);
    }
};

struct ParsingTemporaryAllocation {
    static void *operator new(size_t size) {
        return allocate_zero(size, MEM::PARSE_TEMP);
    }
};

/* Expressions
 */
enum class EXPR {
    __FIRST,
    UNDEFINED = __FIRST,
    AND,
    CMP,
    PLUS,
    MINUS,
    TIMES,
    ARR_LOOK,
    ARR_LEN,
    MSG_SEND,
    NOT,
    ID,
    INT_LIT,
    BOOL_LIT,
    THIS,
    ARR_ALLOC,
    ALLOC,
    __LAST = ALLOC,
};

struct MessageSendData : public ParsingPersistentAllocation {
    const char *id;
    Buf<Expression*> expr_list;
};

struct Expression : public ParsingPersistentAllocation {
    location_t loc;
    EXPR kind;
    union {
        Expression *e1;
        // Integer/True/False Literals.
        int lit_val;
        const char *id;
    };

    bool is_undefined() const {
        return kind == EXPR::UNDEFINED;
    }

    // Pass 1
    Type *accept(DeclarationVisitor *v) {
        assert(0);
    }

    // Pass 2
    Type* accept(MainTypeCheckVisitor *v) {
        return v->visit(this);
    }
};

struct BinaryExpression : public Expression {
    union {
        Expression *e2;
        // We sacrifice an external allocation (hence the pointer)
        // for a not so usual type of Expression.
        MessageSendData *msd;
    };
};



/* Statements
 */
struct Statement : public ParsingPersistentAllocation {
    location_t loc;

    virtual const char *name() const = 0;
    virtual void print() const = 0;
    virtual bool is_undefined() const {
        return false;
    }

    /* Type-checking - Pass 1
    */
    void accept(DeclarationVisitor *v) {
        assert(0);
    }

    /* Type-checking - Pass 2
     */
    virtual void accept(MainTypeCheckVisitor *v) = 0;
};

struct UndefinedStatement : public Statement {
    virtual const char *name() const override {
        return "Undefined (Error)";
    }
    void print() const override;
    bool is_undefined() const override {
        return true;
    }

    /* Type-checking - Pass 2
     */
    void accept(MainTypeCheckVisitor *v) override {
        assert(0);
    }
};

struct BlockStatement : public Statement {
    Buf<Statement*> block;

    const char *name() const override {
        return "Block (Statement)";
    }
    void print() const override;

    /* Type-checking - Pass 2
     */
    void accept(MainTypeCheckVisitor *v) override {
        v->visit(this);
    }
};

struct AssignmentStatement : public Statement {
    const char *id;
    Expression *rhs;

    const char *name() const override {
        return "AssignmentStatement";
    }
    void print() const override;

    /* Type-checking - Pass 2
     */
    void accept(MainTypeCheckVisitor *v) override {
        v->visit(this);
    }
};

struct ArrayAssignmentStatement : public Statement {
    const char *id;
    Expression *index;
    Expression *rhs;

    const char *name() const override {
        return "ArrayAssignmentStatement";
    }
    void print() const override;

    /* Type-checking - Pass 2
     */
    void accept(MainTypeCheckVisitor *v) override {
        v->visit(this);
    }
};

struct IfStatement : public Statement {
    Expression *cond;
    Statement *then, *else_;

    const char *name() const override {
        return "IfStatement";
    }
    void print() const override;

    /* Type-checking - Pass 2
     */
    void accept(MainTypeCheckVisitor *v) override {
        v->visit(this);
    }
};

struct WhileStatement : public Statement {
    Expression *cond;
    Statement *body;

    const char *name() const override {
        return "WhileStatement";
    }
    void print() const override;

    /* Type-checking - Pass 2
     */
    void accept(MainTypeCheckVisitor *v) override {
        v->visit(this);
    }
};

struct PrintStatement : public Statement {
    Expression *to_print;

    const char *name() const override {
        return "PrintStatement";
    }
    void print() const override;

    /* Type-checking - Pass 2
     */
    void accept(MainTypeCheckVisitor *v) override {
        v->visit(this);
    }
};

/* Types (specifiers)
 */
enum class TYSPEC {
    UNDEFINED,
    ARR,
    INT,
    BOOL,
    ID
};

struct Typespec {
    TYSPEC kind;
    const char *id;  // Only for user-defined types.
};

/* Declarations
 */
// vars and parameters
struct LocalDeclaration : public ParsingTemporaryAllocation {
    location_t loc;
    Typespec typespec;
    const char *id;

    LocalDeclaration() {
        typespec.kind = TYSPEC::UNDEFINED;
        id = NULL;
    }

    void make_undefined() {
        id = NULL;
    }

    bool is_undefined() const {
        return id == NULL;
    }

    void print() const;

    /* Type-checking - Pass 1
    */
    Local *accept(DeclarationVisitor *v) {
        return v->visit(this);
    }
};

struct MethodDeclaration : public ParsingTemporaryAllocation {
    location_t loc;
    Typespec typespec;    // return typespec
    const char *id;
    Buf<LocalDeclaration*> params;
    Buf<LocalDeclaration*> vars;
    Buf<Statement*> stmts;
    Expression *ret;

    MethodDeclaration() {
        typespec.kind = TYSPEC::UNDEFINED;
        id = NULL;
        ret = NULL;
    }

    void print() const;

    bool is_undefined() const {
        return id == NULL;
    }

    void make_undefined() {
        id = NULL;
    }

    /* Type-checking - Pass 1
    */
    Method *accept(DeclarationVisitor *v) {
        return v->visit(this);
    }
};

struct TypeDeclaration : public ParsingTemporaryAllocation {
    location_t loc;
    const char *id;
    const char *extends;
    Buf<LocalDeclaration*> vars;
    Buf<MethodDeclaration*> methods;

    TypeDeclaration() {
        this->make_undefined();
    }

    void print() const;

    void make_undefined() {
        id = NULL;
        extends = NULL;
    }

    bool is_undefined() const {
        return id == NULL && extends == NULL;
    }

    /* Type-checking - Pass 1
    */
    void accept(DeclarationVisitor *v) {
        v->visit(this);
    }
};

struct MainClass : public ParsingPersistentAllocation {
    location_t loc;
    const char *id;
    // `LocalDeclaration`s and `Statement`s of main()
    Buf<LocalDeclaration*> vars;
    Buf<Statement*> stmts;

    void make_undefined() {
        id = NULL;
    }

    bool is_undefined() const {
        return id == NULL;
    }

    void print() const;

    /* Type-checking - Pass 1
    */
    void accept(DeclarationVisitor *v) {
        v->visit(this);
    }

    /* Type-checking - Pass 2
     */
    void accept(MainTypeCheckVisitor *v) {
        v->visit(this);
    }
};

struct Goal : public ParsingPersistentAllocation {
    location_t loc;
    MainClass main_class;
    Buf<TypeDeclaration*> type_decls;

    void print() const;

    /* Type-checking - Pass 1
    */
    void accept(DeclarationVisitor *v) {
        v->visit(this);
    }
    
    /* Type-checking - Pass 2
     */
    void accept(MainTypeCheckVisitor *v) {
        v->visit(this);
    }
};


const char *expr_name(EXPR kind);
void expr_print(Expression *e);
void typespec_print(Typespec tspec);
const char *typespec_name(Typespec tspec);

#endif
