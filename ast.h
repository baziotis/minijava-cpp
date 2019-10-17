#ifndef AST_H
#define AST_H

#include "alloc.h"    // MEM, allocate_zero()
#include "buf.h"
#include "typecheck.h"
#include "common.h"

// TODO: Should the `Buf`s have the actual type and not a pointer to it?

// Pre-declarations
struct Expression;

struct ParsingCustomAllocation {
    location_t loc;
    // TODO: We could have a memory arena per class
    // but that would require some changes to the allocator.
    static void *operator new(size_t size) {
        return allocate_zero(size, MEM::PARSE);
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

struct MessageSendData : public ParsingCustomAllocation {
    const char *id;
    Buf<Expression*> expr_list;
};

struct Expression : public ParsingCustomAllocation {
    EXPR kind;
    union {
        Expression *e1;
        // Integer/True/False Literals.
        int lit_val;
        const char *id;
    };

    Type *accept(DeclarationVisitor *v) {
        assert(0);
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
enum struct STMT {
    UNDEFINED,
    BLOCK,
    ASGN,
    ARR_ASGN,
    IF,
    WHILE,
    PRINT
};

struct Statement : public ParsingCustomAllocation {
    STMT kind;

    Statement() { kind = STMT::UNDEFINED; }

    virtual const char *name() const = 0;

    virtual void print() const = 0;

    /* Type-checking
    */
    void accept(DeclarationVisitor *v) {
        assert(0);
    }
};

struct UndefinedStatement : public Statement {
    UndefinedStatement() { kind = STMT::UNDEFINED; }

    virtual const char *name() const override {
        return "Undefined (Error)";
    }

    void print() const override;
};

struct BlockStatement : public Statement {
    Buf<Statement*> block;

    BlockStatement() { kind = STMT::BLOCK; }

    const char *name() const override {
        return "Block (Statement)";
    }

    void print() const override;
};

struct AssignmentStatement : public Statement {
    const char *id;
    Expression *rhs;

    AssignmentStatement() { kind = STMT::ASGN; }

    const char *name() const override {
        return "AssignmentStatement";
    }

    void print() const override;
};

struct ArrayAssignmentStatement : public Statement {
    const char *id;
    Expression *index;
    Expression *rhs;

    ArrayAssignmentStatement() { kind = STMT::ARR_ASGN; }

    const char *name() const override {
        return "ArrayAssignmentStatement";
    }

    void print() const override;

};

struct IfStatement : public Statement {
    Expression *cond;
    Statement *then, *else_;

    IfStatement() { kind = STMT::IF; }

    const char *name() const override {
        return "IfStatement";
    }

    void print() const override;
};

struct WhileStatement : public Statement {
    Expression *cond;
    Statement *body;

    WhileStatement() { kind = STMT::WHILE; }

    const char *name() const override {
        return "WhileStatement";
    }

    void print() const override;
};

struct PrintStatement : public Statement {
    Expression *to_print;

    PrintStatement() { kind = STMT::PRINT; }

    const char *name() const override {
        return "PrintStatement";
    }

    void print() const override;

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
struct LocalDeclaration : public ParsingCustomAllocation {
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

    /* Type-checking
    */
    Local *accept(DeclarationVisitor *v) {
        v->visit(this);
    }
};

struct MethodDeclaration : public ParsingCustomAllocation {
    Typespec typespec;
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

    /* Type-checking
     */
    Method *accept(DeclarationVisitor *v) {
        v->visit(this);
    }
};

struct TypeDeclaration : public ParsingCustomAllocation {
    const char *id;
    const char *extends;
    Buf<LocalDeclaration*> vars;
    Buf<MethodDeclaration*> methods;

    TypeDeclaration() {
        id = NULL;
        extends = NULL;
    }

    void print() const;

    void make_undefined() {
        id = NULL;
        extends = NULL;
    }

    bool is_undefined() const {
        return id == NULL && extends == NULL;
    }

    void accept(DeclarationVisitor *v) {
        v->visit(this);
    }
};

struct MainClass : public ParsingCustomAllocation {
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

    void accept(DeclarationVisitor *v) {
        v->visit(this);
    }

    void print() const;
};

struct Goal : public ParsingCustomAllocation {
    MainClass main_class;
    Buf<TypeDeclaration*> type_decls;

    void print() const;

    void accept(DeclarationVisitor *v) {
        v->visit(this);
    }
};


const char *expr_name(EXPR kind);
void expr_print(Expression *e);
void typespec_print(Typespec tspec);
const char *typespec_name(Typespec tspec);

#endif
