#include <stdio.h>

#include "ast.h"
#include "debug_print.h"

const char *expr_name(EXPR kind) {
    static const char* expr_names[(int)EXPR::__LAST - (int)EXPR::__FIRST + 1];

    expr_names[(int)EXPR::AND] = "AndExpression";
    expr_names[(int)EXPR::CMP] = "CmpExpression";
    expr_names[(int)EXPR::PLUS] = "PlusExpression +";
    expr_names[(int)EXPR::MINUS] = "MinusExpression -";
    expr_names[(int)EXPR::TIMES] = "TimesExpression *";
    expr_names[(int)EXPR::ARR_LOOK] = "ArrayLookup";
    expr_names[(int)EXPR::ARR_LEN] = "ArrayLength";
    expr_names[(int)EXPR::MSG_SEND] = "MessageSend";
    expr_names[(int)EXPR::NOT] = "NotExpression";
    expr_names[(int)EXPR::ID] = "Identifier";
    expr_names[(int)EXPR::INT_LIT] = "IntegerLiteral";
    expr_names[(int)EXPR::BOOL_LIT] = "BoolLiteral";
    expr_names[(int)EXPR::THIS] = "ThisExpression";
    expr_names[(int)EXPR::ARR_ALLOC] = "ArrayAllocationExpression";
    expr_names[(int)EXPR::ALLOC] = "AllocationExpression";
    expr_names[(int)EXPR::UNDEFINED] = "Undefined (Error)";

    return expr_names[(int)kind];
}

static void red_on() {
    printf("\x1b[31m");
}

static void red_off() {
    printf("\x1b[0m");
}

void expr_print(Expression *e) {
    LOG_SCOPE;
    if (!e) return;
    switch (e->kind) {
    case EXPR::PLUS:
    case EXPR::MINUS:
    case EXPR::TIMES:
    case EXPR::CMP:
    case EXPR::AND:
    case EXPR::ARR_LOOK:
    {
        BinaryExpression *be = (BinaryExpression*) e;
        debug_print("(");
        printf("%s\n", expr_name(be->kind));
        expr_print(be->e1);
        expr_print(be->e2);
        debug_line(")");
    } break;
    case EXPR::THIS:
    {
        debug_line("(ThisExpression)");
    } break;
    case EXPR::INT_LIT:
    case EXPR::BOOL_LIT:
    {
        debug_print("(");
        printf("%s %d)\n", expr_name(e->kind), e->lit_val);
    } break;
    case EXPR::MSG_SEND:
    {
        BinaryExpression *be = (BinaryExpression*) e;
        debug_line("(MessageSend");
        expr_print(be->e1);
        // For some reason, debug_line does not work here
        debug_print("- Id: %s\n", be->msd->id);
        debug_print("- Expression list:\n");
        LOG_SCOPE;
        for (Expression* e2 : be->msd->expr_list) {
            expr_print(e2);
        }
        debug_line(")");
    } break;
    case EXPR::ID:
    {
        debug_print("(");
        printf("%s %s)\n", expr_name(e->kind), e->id);
    } break;
    case EXPR::UNDEFINED:
    {
        debug_print("(");
        printf("Undefined ");
        red_on();
        printf("(Error)");
        red_off();
        printf(")\n");
    } break;
    case EXPR::ARR_LEN:
    case EXPR::ARR_ALLOC:
    case EXPR::NOT:
    {
        debug_print("(%s\n", expr_name(e->kind));
        expr_print(e->e1);
        debug_line(")");
    } break;
    default: printf("Expression print error: %s\n", expr_name(e->kind)); assert(0);
    }
}

/* Statements Priting
 */
void UndefinedStatement::print() const {
    LOG_SCOPE;
    debug_line("(UndefinedStatement (Error))");
}

void BlockStatement::print() const {
    LOG_SCOPE;
    debug_line("(BlockStatement");
    for (const Statement *stmt : this->block) {
        stmt->print();
    }
    debug_line(")");
}

void AssignmentStatement::print() const {
    LOG_SCOPE;
    debug_line("(AssignmentStatement");
    debug_print("- Identifier: %s\n", this->id);
    expr_print(this->rhs);
    debug_line(")");
}

void ArrayAssignmentStatement::print() const {
    LOG_SCOPE;
    debug_line("(ArrayAssignmentStatement");
    debug_print("- Identifier: %s\n", this->id);
    expr_print(this->index);
    expr_print(this->rhs);
    debug_line(")");
}

void IfStatement::print() const {
    LOG_SCOPE;
    debug_line("(IfStatement");
    debug_print("- Cond: \n");
    {
        LOG_SCOPE;
        expr_print(this->cond);
    }
    debug_print("- Then: \n");
    {
        LOG_SCOPE;
        this->then->print();
    }
    debug_print("- Else: \n");
    {
        LOG_SCOPE;
        this->else_->print();
    }
    debug_line(")");
}

void WhileStatement::print() const {
    LOG_SCOPE;
    debug_line("(WhileStatement");
    debug_print("- Cond: \n");
    {
        LOG_SCOPE;
        expr_print(this->cond);
    }
    debug_print("- Body: \n");
    {
        LOG_SCOPE;
        this->body->print();
    }
    debug_line(")");
}

void PrintStatement::print() const {
    LOG_SCOPE;
    debug_line("(PrintStatement");
    expr_print(this->to_print);
    debug_line(")");
}

/* Typespec printing
 */
void typespec_print(Typespec tspec) {
    switch (tspec.kind) {
    case TYSPEC::UNDEFINED:
    {
        printf("Undefined Typespec (Error)");
    } break;
    case TYSPEC::ARR:
    {
        printf("int[]");
    } break;
    case TYSPEC::INT:
    {
        printf("int");
    } break;
    case TYSPEC::BOOL:
    {
        printf("boolean");
    } break;
    case TYSPEC::ID:
    {
        printf("Id Type: %s", tspec.id);
    } break;
    default: assert(0);
    }
} 

const char *typespec_name(Typespec tspec) {
    switch (tspec.kind) {
    case TYSPEC::UNDEFINED: return "Undefined Typespec (Error)";
    case TYSPEC::ARR: return "int[]";
    case TYSPEC::INT: return "int";
    case TYSPEC::BOOL: return "boolean";
    case TYSPEC::ID: return "Id Type";
    default: assert(0);
    }
}


/* Declarations
 */
void TypeDeclaration::print() const {
    LOG_SCOPE;
    if (this->is_undefined()) {
        debug_line("Undefined TypeDeclaration (Error)");
        return;
    }
    debug_print("(TypeDeclaration %s", this->id);
    if (this->extends) {
        printf(" _EXTENDS_ %s\n", this->extends);
    } else {
        printf("\n");
    }
    {
        LOG_SCOPE;
        debug_print("Member Vars:\n");
        for (LocalDeclaration *ld : this->vars) {
            ld->print();
        }
    }
    {
        LOG_SCOPE;
        debug_print("Member Methods:\n");
        {
            for (MethodDeclaration *md : this->methods) {
                md->print();
            }
        }
    }
    debug_print(")\n");
}

void MethodDeclaration::print() const {
    LOG_SCOPE;
    if (this->is_undefined()) {
        debug_line("Undefined MethodDeclaration (Error)");
        return;
    }
    debug_print("(MethodDeclaration %s\n", this->id);
    {
        LOG_SCOPE;
        debug_print("Parameters:\n");
        {
            for (LocalDeclaration *ld: this->params) {
                ld->print();
            }
        }
    }
    {
        LOG_SCOPE;
        debug_print("Body:\n");
        {
            for (Statement *s: this->stmts) {
                if (s->kind != STMT::UNDEFINED) {
                    s->print();
                }
            }
        }
    }
    {
        LOG_SCOPE;
        debug_print("Return:\n");
        {
            expr_print(this->ret);
        }
    }
    debug_print(")\n");
}

void LocalDeclaration::print() const {
    LOG_SCOPE;
    typespec_print(this->typespec);
    printf(": %s\n", this->id);
}

void MainClass::print() const {
    LOG_SCOPE;
    debug_print("(MainClass %s\n", this->id);
    {
        LOG_SCOPE;
        debug_print("VarDeclarations of `main`:\n");
        for (LocalDeclaration *ld : this->vars) {
            ld->print();
        }
    }
    {
        LOG_SCOPE;
        debug_print("Statements of `main`:\n");
        for (Statement *s : this->stmts) {
            s->print();
        }
    }
    debug_print(")\n");
}

void Goal::print() const {
    LOG_SCOPE;
    this->main_class.print();
    for (TypeDeclaration *type_decl : this->type_decls) {
        type_decl->print();
    }
}
