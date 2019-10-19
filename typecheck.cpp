#include <assert.h>
#include <stdio.h>

#include "ast.h"
#include "alloc.h"
#include "common.h"
#include "debug_print.h"
#include "error.h"          // log()
                            // WARNING: error.h gives access to the global `loc`
#include "hash_table.h"
#include "str_intern.h"

extern config_t config;

void typecheck_init() {
    set_indent_char('*');
}

/// Fill the type_table.
TypeTable install_type_declarations(Goal *goal) {
    DeclarationVisitor decl_visitor(goal->type_decls.len);
    goal->accept(&decl_visitor);
    
    /* Use with test.java */
    // TODO: remove that
    //TypeTable type_table = decl_visitor.type_table;
    //Type *type = type_table.find(str_intern("A"));
    //assert(type);
    //IdType *id_type = type->is_IdType();
    //assert(id_type);
    //Field *field = id_type->fields.find(str_intern("b"));
    //assert(field);
    //Method *method = id_type->methods.find(str_intern("test"));
    //assert(method);

    //assert(id_type->fields.find(str_intern("c")) == NULL);
    //assert(id_type->methods.find(str_intern("other")) == NULL);

    //Type *D_type = type_table.find(str_intern("D"));
    //assert(D_type->is_IdType());
    //IdType *C_type = type_table.find(str_intern("C"));
    //assert(C_type->is_IdType());
    //Field *d2_field = C_type->fields.find(str_intern("d2"));
    //assert(d2_field->type == D_type);

    //// Test inheritance (A is supposed to inherit from B)
    //IdType *B_type = type_table.find(str_intern("B"));
    //IdType *A_type = type_table.find(str_intern("A"));
    //assert(A_type);
    //assert(B_type);
    //assert(A_type->is_IdType());
    //assert(B_type->is_IdType());
    //assert(A_type->parent);
    //assert(A_type->parent == B_type);


    // Free memory that was used for allocating objects
    // for parsing that doenot persist in pass 2 of type-checking.
    deallocate(MEM::PARSE_TEMP);

    return decl_visitor.type_table;
}

void full_typecheck(Goal *goal, TypeTable type_table) {
    MainTypeCheckVisitor main_visitor(type_table);
    goal->accept(&main_visitor);
}

void typecheck(Goal goal) {
    typecheck_init();
    // Pass 1
    TypeTable type_table = install_type_declarations(&goal);
    if (config.log) {
        printf("\n");
    }
    // Pass 2
    full_typecheck(&goal, type_table);
}


/* Declaration Visitor
 */
IdType* DeclarationVisitor::id_to_type(const char *id) {
    // Check if it already exists.
    IdType *type = this->type_table.find(id);
    if (type) {
        return type;
    }
    // Otherwise construct a new one and
    // save it in the table.
    type = new IdType(id);
    this->type_table.insert(type->id, type);
    return type;
}

// This is member of the visitor so that we have
// access to the type table.
Type* DeclarationVisitor::typespec_to_type(Typespec tspec) {
    switch (tspec.kind) {
    case TYSPEC::UNDEFINED: assert(0);
    case TYSPEC::INT: return type_table.int_type;
    case TYSPEC::ARR: return type_table.int_arr_type;
    case TYSPEC::BOOL: return type_table.bool_type;
    case TYSPEC::ID: return this->id_to_type(tspec.id);
    default: assert(0);
    }
}

void TypeTable::initialize(size_t n) {
    type_table.reserve(n);
    undefined_type = new Type(TY::UNDEFINED);
    bool_type = new Type(TY::BOOL);
    int_type = new Type(TY::INT);
    int_arr_type = new Type(TY::ARR);
}

DeclarationVisitor::DeclarationVisitor(size_t ntype_decls) {
    // IMPORTANT: Before installing a type, the user
    // is responsible for checking if it exists.
    this->type_table.initialize(ntype_decls);
}

void DeclarationVisitor::visit(Goal *goal) {
    LOG_SCOPE;
    debug_print("Pass1::Goal\n");
    goal->main_class.accept(this);
    for (TypeDeclaration *type_decl : goal->type_decls) {
        type_decl->accept(this);
    }
}

void DeclarationVisitor::visit(MainClass *main_class) {
    LOG_SCOPE;
    debug_print("Pass1::MainClass: %s\n", main_class->id);
}

// Note - IMPORTANT: 
// A lot of IdTypes just start undefined (i.e. kind == TY::UNDEFINED)
// if we have seen a declaration with this type but we have
// not yet seen a definition. For example:
//     int test(A a)
// if we have not yet have seen the definition of A.
// We install (i.e allocate) a type (if one does not exist already for id `A`)
// but we just keep it undefined (and keep a pointer to it).
// If we then _see_
//     class A { ...
// we don't allocate a new type (note below that we're searching the table).
// We do the changes in the same memory that was allocated when we were in `test(A a)`.
// And we change the type from undefined.

// If we never see the definition of `A`, it will be caught in pass 2 as described below.

// Essentially, this almost eliminates searches in the type table in the pass 2.
// We just check the pointer and if the type was never defined, it will have
// remained undefined and we issue an error. Otherwise, the definition of this
// type will be in the same memory location.

void DeclarationVisitor::visit(TypeDeclaration *type_decl) {
    LOG_SCOPE;
    assert(!type_decl->is_undefined());
    print_indentation();
    debug_log(type_decl->loc, "Pass1::TypeDeclaration: ", type_decl->id, "\n");
    IdType *type = this->type_table.find(type_decl->id);
    // If it exists and it's declared (i.e. we have processed
    // a type with the same `id`), then we have redeclaration error.
    if (type) {
        if (type->is_defined()) {
            typecheck_error(type_decl->loc, "Type with id: `", type_decl->id,
                            "` has already been declared");
            return;
        } else {
            type->set_sizes(type_decl->vars.len, type_decl->methods.len);
        }
    } else {
        type = new IdType(type_decl->id, type_decl->vars.len, type_decl->methods.len);
        // TODO: insert it as undefined if it's not correct.
        this->type_table.insert(type->id, type);
    }
    // Handle inheritance
    if (type_decl->extends) {
        IdType *parent = this->id_to_type(type_decl->extends);
        type->set_parent(parent);
    }
    for (LocalDeclaration *ld : type_decl->vars) {
        // Check redeclaration
        Field *field = type->fields.find(ld->id);
        if (field) {
            typecheck_error(ld->loc, "In class with id: `", type_decl->id,
                            "`, redeclaration of field with id: `", field->id, "`");
        } else {
            field = ld->accept(this);
            //field->type->print();
            type->fields.insert(field->id, field);
        }
    }
    for (MethodDeclaration *md : type_decl->methods) {
        // Check redeclaration
        Method *method = type->methods.find(md->id);
        if (method) {
            typecheck_error(md->loc, "In class with id: `", type_decl->id,
                            "`, redeclaration of method with id: `", method->id, "`");
        } else {
            method = md->accept(this);
            //method->print();
            type->methods.insert(method->id, method);
        }
    }
}

Local *DeclarationVisitor::visit(LocalDeclaration *local_decl) {
    // Note: It's responsibility of the one who calls this visit
    // to have assured that a local declaration with the same
    // id does not exist.
    LOG_SCOPE;
    assert(!local_decl->is_undefined());
    print_indentation();
    debug_log(local_decl->loc, "LocalDeclaration: ", local_decl->id, "\n");
    Type *type = typespec_to_type(local_decl->typespec);
    Local *local = new Local(local_decl->id, type);
    return local;
}

const char *DeclarationVisitor::gen_id() {
    static int count = 0;
    char buf[64];
    sprintf(buf, "a%d", count);
    count++;
    return str_intern(buf);
}

Method *DeclarationVisitor::visit(MethodDeclaration *method_decl) {
    // Note: It's responsibility of the one who calls this visit
    // to have assured that a method with the same id does not exist.
    LOG_SCOPE;
    assert(!method_decl->is_undefined());
    print_indentation();
    debug_log(method_decl->loc, "MethodDeclaration: ", method_decl->id, "\n");
    Method *method = new Method(method_decl);
    method->ret_type = this->typespec_to_type(method_decl->typespec);
    for (LocalDeclaration *par : method_decl->params) {
        // We accept the param anyway. But in the case that there is
        // another param with the same name, we still insert this
        // (so that the rest of type-checking can sort of continue) but
        // we have to generate a (dummy) id for it.
        Param *param = par->accept(this);
        if (method->locals.find(par->id)) {
            typecheck_error(par->loc, "Parameter `", par->id, "` is already defined",
                            " in method `", method_decl->id, "`");
            param->id = gen_id();
        } else {
            param->type->print();
        }
        method->locals.insert(param->id, param);
    }
    for (LocalDeclaration *var : method_decl->vars) {
        if (method->locals.find(var->id)) {
            typecheck_error(var->loc, "Variable `", var->id, "` is already defined",
                            " in method `", method_decl->id, "`");
        } else {
            Var *v = var->accept(this);
            v->type->print();
            method->locals.insert(v->id, v);
        }
    }
    return method;
}

/* Main TypeCheck Visitor (Pass 2)
 */
void MainTypeCheckVisitor::visit(Goal *goal) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::Goal\n");
    goal->main_class.accept(this);
    for (IdType *type : this->type_table) {
        type->accept(this);
    }
}

void MainTypeCheckVisitor::visit(MainClass *main_class) {
    LOG_SCOPE;
    //debug_print("MainTypeCheck::MainClass\n");
}

void MainTypeCheckVisitor::visit(IdType *type) {
    LOG_SCOPE;
    assert(type->is_IdType());
    debug_print("MainTypeCheck::IdType %s\n", type->id);

    this->curr_class = type;
    for (Method *method : type->methods) {
        method->accept(this);
    }
    this->curr_class = NULL;
}

void MainTypeCheckVisitor::visit(Method *method) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::Method %s\n", method->id);
    this->curr_method = method;
    for (Statement *stmt : method->stmts) {
        stmt->accept(this);
    }
    // An undefined return expression may actually end up here.
    // Check parse.cpp
    if (!method->ret_expr->is_undefined()) {
        Type *ret_type = method->ret_expr->accept(this);
        assert(ret_type);
        if (ret_type != method->ret_type) {
            typecheck_error(method->ret_expr->loc, "The type: `", ret_type->name(),
                            "` of the return expression does not match the ",
                            "return type: `", method->ret_type->name(),
                            "` of method: `", method->id, "`");
        }
    }
    this->curr_method = NULL;
}

static Type *lookup_id(const char *id, Method *method, IdType *cls) {
    // Check current method's locals (vars and params).
    Local *local = method->locals.find(id);
    if (local) {
        return local->type;
    }
    // Check current class's fields.
    local = cls->fields.find(id);
    if (local) {
        return local->type;
    }
    // Check parent's fields.
    while (cls->parent) {
        cls = cls->parent;
        local = cls->fields.find(id);
        if (local) {
            return local->type;
        }
    }
    return NULL;
}

static Method *lookup_method(const char *id, IdType *cls) {
    // Check current class's methods
    Method *method = cls->methods.find(id);
    if (method) {
        return method;
    }
    // Check parent's methods.
    while (cls->parent) {
        cls = cls->parent;
        method = cls->methods.find(id);
        if (method) {
            return method;
        }
    }
    return NULL;
}

// IMPORTANT: DO NOT check if a type is undefined with equality test with
// type_table.undefined_type. Check a note above on a full explanation.
// A lot of types can have remained undefined (either because we never saw a definition
// or the definition was invalid) but are not pointing to
// type_table.undefined_type. type_table.undefined_type is only for denoting
// an undefined type in general (e.g. an expression has undefined type, return
// type_table.undefined_type as its type. The caller checks for equality
// with type_table.undefined_type to see if it was valid).

Type* MainTypeCheckVisitor::visit(Expression *expr) {
    LOG_SCOPE;
    assert(!expr->is_undefined());
    
    // Used in binary expression cases.
    BinaryExpression *be = (BinaryExpression *) expr;

    switch (expr->kind) {
    case EXPR::BOOL_LIT:
    {
        debug_print("MainTypeCheck::BoolExpression\n");
        return this->type_table.bool_type;
    } break;
    case EXPR::ID:
    {
        debug_print("MainTypeCheck::IdExpression: %s\n", expr->id);
        assert(this->curr_method);
        assert(this->curr_class);
        Type *type = lookup_id(expr->id, this->curr_method, this->curr_class);
        if (!type) {
            typecheck_error(expr->loc, "In identifier expression, Identifier: `",
                            expr->id, "` does is not defined.");
            return this->type_table.undefined_type;
        }
        return type;
    } break;
    case EXPR::INT_LIT:
    {
        debug_print("MainTypeCheck::IntegerExpression\n");
        return this->type_table.int_type;
    } break;
    case EXPR::THIS:
    {
        debug_print("MainTypeCheck::ThisExpression\n");
        assert(this->curr_class);
        return this->curr_class;
    } break;
    case EXPR::ALLOC:
    {
        debug_print("MainTypeCheck::AllocationExpression\n");
        // Find the type of the Identifier.
        IdType *type = this->type_table.find(expr->id);
        if (!type || type->kind == TY::UNDEFINED) {
            typecheck_error(expr->loc, "In allocation expression, Identifier: `",
                            expr->id, "` does not denote a known type");
            return this->type_table.undefined_type;
        }
        if (!type->is_IdType()) {
            typecheck_error(expr->loc, "In allocation expression, Identifier: `",
                            expr->id, "` does not denote a user-defined type");
            return this->type_table.undefined_type;
        }
        return type;
    } break;
    case EXPR::ARR_ALLOC:
    {
        debug_print("MainTypeCheck::ArrayAllocationExpression\n");
        assert(expr->e1);
        Type *index_type = expr->e1->accept(this);
        index_type->print();
        if (index_type != this->type_table.int_type) {
            typecheck_error(expr->loc, "In array allocation expression, ",
                            "the index expression must be of integer type.");
            return this->type_table.undefined_type;
        }
        return this->type_table.int_arr_type;
    } break;
    case EXPR::ARR_LEN:
    {
        debug_print("MainTypeCheck::LengthExpression\n");
        assert(expr->e1);
        Type *arr = expr->e1->accept(this);
        if (arr != this->type_table.int_arr_type) {
            typecheck_error(expr->loc, "In array length expression, ",
                            "the dereferenced id must be of integer array type.");
            return this->type_table.undefined_type;
        }
        return this->type_table.int_type;
    } break;
    case EXPR::NOT:
    {
        debug_print("MainTypeCheck::NotExpression\n");
        assert(expr->e1);
        Type *ty = expr->e1->accept(this);
        if (ty != this->type_table.bool_type) {
            typecheck_error(expr->loc, "Bad operand for unary operator `!`. Operand ",
                            "of boolean type was expected, found: `", ty->name(), "`");
            return this->type_table.undefined_type;
        }
        return this->type_table.bool_type;
    } break;

/*----------- BINARY EXPRESSIONS ----------------*/

    case EXPR::AND:
    {
        debug_print("MainTypeCheck::AndExpression\n");
        assert(be->e1);
        assert(be->e2);
        Type *ty1 = be->e1->accept(this);
        Type *ty2 = be->e2->accept(this);
        bool is_correct = true;

        if (ty1 != this->type_table.bool_type) {
            typecheck_error(expr->loc, "Bad left operand for binary operator `&&`. Operand ",
                            "of boolean type was expected, found: `", ty1->name(), "`");
            is_correct = false;
        }
        if (ty2 != this->type_table.bool_type) {
            typecheck_error(expr->loc, "Bad right operand for binary operator `&&`. Operand ",
                            "of boolean type was expected, found: `", ty2->name(), "`");
            is_correct = false;
        }
        if (is_correct) {
            return this->type_table.bool_type;
        }
        return this->type_table.undefined_type;
    } break;


    // If e1, e2 in the CMP, PLUS, MINUS, TIMES are NOT expressions
    // should not be considered a semantic error but an internal compiler
    // error as they should have been handled in parsing.

    case EXPR::CMP:
    {
        debug_print("MainTypeCheck::CmpExpression\n");
        assert(be->e1);
        assert(be->e2);
        assert(be->e1->kind != EXPR::NOT);
        assert(be->e2->kind != EXPR::NOT);
        Type *ty1 = be->e1->accept(this);
        Type *ty2 = be->e2->accept(this);
        bool is_correct = true;

        if (ty1 != this->type_table.int_type) {
            typecheck_error(expr->loc, "Bad left operand for binary operator `<`. Operand ",
                            "of int type was expected, found: `", ty2->name(), "`");
            is_correct = false;
        }
        if (ty2 != this->type_table.int_type) {
            typecheck_error(expr->loc, "Bad right operand for binary operator `<`. Operand ",
                            "of int type was expected, found: `", ty2->name(), "`");
            is_correct = false;
        }
        if (is_correct) {
            return this->type_table.bool_type;
        }
        return this->type_table.undefined_type;
    } break;
    case EXPR::PLUS:
    case EXPR::MINUS:
    case EXPR::TIMES:
    {
        assert(be->e1);
        assert(be->e2);
        assert(be->e1->kind != EXPR::NOT);
        assert(be->e2->kind != EXPR::NOT);
        Type *ty1 = be->e1->accept(this);
        Type *ty2 = be->e2->accept(this);
        bool is_correct = true;

        int op;
        if (be->kind == EXPR::PLUS) {
            op = '+';
            debug_print("MainTypeCheck::PlusExpression\n");
        } else if (be->kind == EXPR::MINUS) {
            op = '-';
            debug_print("MainTypeCheck::MinusExpression\n");
        } else {
            op = '*';
            debug_print("MainTypeCheck::TimesExpression\n");
        }

        if (ty1 != this->type_table.int_type) {
            typecheck_error(expr->loc, "Bad left operand for binary operator `", (char)op ,
                            "`. Operand of int type was expected, found: `",
                            ty1->name(), "`");
            is_correct = false;
        }
        if (ty2 != this->type_table.int_type) {
            typecheck_error(expr->loc, "Bad right operand for binary operator `", (char)op ,
                            "`. Operand of int type was expected, found: `",
                            ty1->name(), "`");
            is_correct = false;
        }
        if (is_correct) {
            return this->type_table.int_type;
        }
        return this->type_table.undefined_type;
    } break;
    case EXPR::ARR_LOOK:
    {
        debug_print("MainTypeCheck::ArrayLookupExpression\n");
        assert(be->e1);
        assert(be->e2);
        assert(be->e1->kind != EXPR::NOT);
        assert(be->e2->kind != EXPR::NOT);
        Type *ty1 = be->e1->accept(this);
        Type *ty2 = be->e2->accept(this);
        bool is_correct = true;

        if (ty1 != this->type_table.int_arr_type) {
            typecheck_error(expr->loc, "Bad left operand for index operator `[]`. ",
                            "Operand of int array type was expected, found: ",
                            ty1->name(), "`");
            is_correct = false;
        }
        if (ty2 != this->type_table.int_type) {
            typecheck_error(expr->loc, "Bad index expression for index operator `[]`. ",
                            "Operand of int type was expected, found: `",
                            ty1->name(), "`");
            is_correct = false;
        }
        if (is_correct) {
            return this->type_table.int_type;
        }
        return this->type_table.undefined_type;
    } break;
    case EXPR::MSG_SEND:
    {
        debug_print("MainTypeCheck::MessageSendExpression\n");
        assert(be->e1);
        assert(be->e1->kind != EXPR::NOT);
        IdType *type = (IdType*) be->e1->accept(this);
        assert(type);

        if (type->kind == TY::UNDEFINED) {
            typecheck_error(expr->loc, "In message send expression, Identifier: `",
                            expr->id, "` does not denote a known type");
            return this->type_table.undefined_type;
        }
        if (!type->is_IdType()) {
            typecheck_error(expr->loc, "Bad dereferenced operand for message",
                            "send operator `.`. ", "Operand of user-defined ",
                            "type was expected, found: `", type->name(), "`");
            return this->type_table.undefined_type;
        }

        assert(this->curr_class);
        Method *method = lookup_method(be->msd->id, type);

        if (!method) {
            typecheck_error(expr->loc, "Type with id: `", type->id, "` does not ",
                            "have a method with id: `", be->msd->id, "`");
            return this->type_table.undefined_type;
        }

        if (be->msd->expr_list.len != method->param_len) {
            typecheck_error(expr->loc, "The no. of arguments (",
                                be->msd->expr_list.len, ") does not match the ",
                                "number of formal parameters (", method->param_len,
                                ") of method with id: `", method->id, "`");
            return method->ret_type;
        }
        
        size_t formal_param_counter = 0;
        for (Expression *e : be->msd->expr_list) {
            Type *ety = e->accept(this);
            Type *formal_type = method->locals[formal_param_counter]->type;
            if (ety != formal_type) {
                typecheck_error(expr->loc, "Argument no. ", formal_param_counter + 1,
                                " with type: `", ety->name(), "` does not match ",
                                "formal parameter type: `", formal_type->name(),
                                "` in method with id: `", method->id,
                                "` in message send expression");
                return method->ret_type;
            }
            ++formal_param_counter;
        }
        assert(formal_param_counter == method->param_len);
        return method->ret_type;
    } break;
    default: assert(0); return this->type_table.undefined_type;
    }
}

/* Statements
 */
// TODO: Global error counting to known whether there was error in statement.
void MainTypeCheckVisitor::visit(BlockStatement *block_stmt) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::BlockStatement\n");
    for (Statement *stmt : block_stmt->block) {
        stmt->accept(this);
    }
}

void MainTypeCheckVisitor::visit(AssignmentStatement *asgn_stmt) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::AssignmentStatement\n");
    assert(this->curr_method);
    assert(this->curr_class);
    Type *lhs = lookup_id(asgn_stmt->id, this->curr_method, this->curr_class);
    if (!lhs) {
        // Of course, with variable we mean also parameter and field.
        typecheck_error(asgn_stmt->loc, "In assignment statement, variable: `",
                        asgn_stmt->id, "` is not defined");
    }
    assert(asgn_stmt->rhs);
    Type *rhs = asgn_stmt->rhs->accept(this);
    if (lhs != rhs) {
        typecheck_error(asgn_stmt->loc, "In assignment statement, ",
                        "the left-hand-side type: `", lhs->name(),
                        "` does not match that of the right-hand-side: `",
                        rhs->name(), "`");
    }
}

void MainTypeCheckVisitor::visit(ArrayAssignmentStatement *arr_asgn_stmt) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::ArrayAssignmentStatement\n");
    assert(this->curr_method);
    assert(this->curr_class);
    Type *arr = lookup_id(arr_asgn_stmt->id, this->curr_method, this->curr_class);
    if (!arr) {
        typecheck_error(arr_asgn_stmt->loc, "In array assignment statement, array: `",
                        arr_asgn_stmt->id, "` is not defined");
    }
    assert(arr_asgn_stmt->index);
    Type *index = arr_asgn_stmt->index->accept(this);
    if (index != this->type_table.int_type) {
        typecheck_error(arr_asgn_stmt->loc, "In array assignment statement, the ",
                        "index expression must have `int` type but has: `",
                        index->name(), "`");
    }
    assert(arr_asgn_stmt->rhs);
    Type *rhs = arr_asgn_stmt->rhs->accept(this);
    if (rhs != this->type_table.int_type) {
        typecheck_error(arr_asgn_stmt->loc, "In array assignment statement, the ",
                        "right-hand-side expression must have `int` type but has: `",
                        rhs->name(), "`");
    }
}

void MainTypeCheckVisitor::visit(IfStatement *if_stmt) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::IfStatement\n");
    Type *cond = if_stmt->cond->accept(this);
    if (cond != this->type_table.bool_type) {
        typecheck_error(if_stmt->loc, "In if statement, the ",
                        "condition expression must have `boolean` type but has: `",
                        cond->name(), "`");
    }
    assert(if_stmt->then);
    assert(if_stmt->else_);
    if_stmt->then->accept(this);
    if_stmt->else_->accept(this);
}

void MainTypeCheckVisitor::visit(WhileStatement *while_stmt) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::WhileStatement\n");
    Type *cond = while_stmt->cond->accept(this);
    if (cond != this->type_table.bool_type) {
        typecheck_error(while_stmt->loc, "In while statement, the ",
                        "condition expression must have `boolean` type but has: `",
                        cond->name(), "`");
    }
    assert(while_stmt->body);
    while_stmt->body->accept(this);
}

void MainTypeCheckVisitor::visit(PrintStatement *print_stmt) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::PrintStatement\n");
    // A print statement can have any type, we visit it only for
    // type-checking purposes.
    print_stmt->to_print->accept(this);
}

/* Types
 */
void Type::print() const {
    LOG_SCOPE;
    switch (this->kind) {
    case TY::UNDEFINED: {
        red_on();
        debug_print("Undefined Type (Error)\n");
        red_off();
    } break;
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

Method::Method(MethodDeclaration *method_decl) {
    id = method_decl->id;
    locals.reserve(method_decl->params.len + method_decl->vars.len);
    param_len = method_decl->params.len;
    stmts = method_decl->stmts;
    ret_expr = method_decl->ret;
}

void Method::print() const {
    LOG_SCOPE;
    debug_print("Method name: %s\n", this->id);
}

void IdType::print() const {
    LOG_SCOPE;
    debug_print("IdType: %s\n", this->id);
}
