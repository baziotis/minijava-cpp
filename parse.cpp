#include "ast.h"
#include "debug_print.h"    // set_indent_char()
#include "error.h"
#include "lex.h"            // lexer_init(), next_token()
#include "str_intern.h"     // str_intern()
#include "tokens.h"         // TOK

extern token_t token;

static bool EOI_has_been_found;

static const char *main_;
static const char *length_;

static bool is_token(TOK kind) {
    return token.kind == kind;
}

// TODO: location_t assignment can be moved to a constructor.

//Uncomment to trace the line
//of where an expect_token()
// was called.
/*
#define expect_token(kind) \
    printf("%d\n", __LINE__); \
    expect_token1(kind); \
*/

/// If the current token is not of kind `kind`, generate an
/// error and return `false`. Otherwise, return true.
/// In both cases, advance to the next token.
static bool expect_token(TOK kind, bool advance = false) {
    if (EOI_has_been_found) return false;
    if (is_token(TOK::EOI)) {
        EOI_has_been_found = true;
    }
    bool res;
    if (is_token(kind)) {
        next_token();
        return true;
    } else {
        syntax_error("Expected `", kind, "`, found `", token, "`");
        res = false;
    }
    if (advance) {
        next_token();
    }
    return res;
}

static bool expect_token_in_rule(TOK kind, const char *rule, bool advance = false) {
    if (EOI_has_been_found) return false;
    if (is_token(TOK::EOI)) {
        EOI_has_been_found = true;
    }
    bool res;
    if (is_token(kind)) {
        next_token();
        return true;
    } else {
        syntax_error("Expected `", kind, "` in: ", rule, ", found `", token, "`");
        res = false;
    }
    if (advance) {
        next_token();
    }
    return res;
}

static bool expect_interned_id(const char *id) {
    if (EOI_has_been_found) return false;
    if (is_token(TOK::EOI)) {
        EOI_has_been_found = true;
    }
    bool res;
    if (is_token(TOK::ID)) {
        if (token.id == id) {
            next_token();
            return true;
        } else {
            syntax_error("Expected `", id, "`, found `", token.id, "`");
            res = false;
        }
    } else {
        syntax_error("Expected `", id, "`, found `", token.kind, "`");
        res = false;
    }
    next_token();
    return res;
}
    
/// If the the current token is of `kind`, return `true`
/// and advance. Otherwise, return `false`.
static bool match_token(TOK kind) {
    if (is_token(kind)) {
        next_token();
        return true;
    }
    return false;
}

static bool is_op(TOK kind) {
    return kind >= TOK::__FIRST_OP && kind <= TOK::__LAST_OP;
}
    
/// Skip tokens until you find one whose `category` is in `set`
static void skip_tokens(TOK kind) {
    int num_errors = 0;
    constexpr int lim = 5;
    TOK last;
    while (!is_token(TOK::EOI) && token.kind != kind) {
        if (num_errors == 0) {
            printf(";  skipping ");
        }
        ++num_errors;
        if (num_errors < lim) {
            if (num_errors > 1) {
                printf(", ");
            }
            log("`", token, "`");
        }
        last = token.kind;
        next_token();
    }
    next_token();
    if (num_errors >= lim) {
        log("... ", last);
    }
    printf("\n");
}

// Pre-declarations
static Expression *parse_expr();
static Statement *parse_stmt();


/* Expressions
 */
static Expression *parse_expr_primary() {
    if (is_token(TOK::LPAR)) {
        next_token();
        Expression *expr = parse_expr();
        expect_token(TOK::RPAR);
        return expr;
    }
    Expression *expr = new Expression;
    expr->loc = loc;
    switch (token.kind) {
    case TOK::FALSE:
    {
        next_token();
        expr->kind = EXPR::BOOL_LIT;
        expr->lit_val = 0;
    } break;
    case TOK::ID:
    {
        expr->kind = EXPR::ID;
        expr->id = token.id;
        next_token();
    } break;
    case TOK::INTLIT:
    {
        expr->kind = EXPR::INT_LIT;
        expr->lit_val = token.val;
        next_token();
    } break;
    case TOK::THIS:
    {
        next_token();
        expr->kind = EXPR::THIS;
    } break;
    case TOK::NEW:
    {
        next_token();
        if (match_token(TOK::INT)) { // ArrayAllocationExpression
            expr->kind = EXPR::ARR_ALLOC;
            expect_token_in_rule(TOK::LBRACKET, "array allocation expression");
            expr->e1 = parse_expr();
            expect_token_in_rule(TOK::RBRACKET, "array allocation expression");
        } else if (is_token(TOK::ID)) {
            expr->kind = EXPR::ALLOC;
            expr->id = token.id;
            next_token();
            expect_token_in_rule(TOK::LPAR, "allocation expression");
            expect_token_in_rule(TOK::RPAR, "allocation expression");
        }
    } break;
    case TOK::TRUE:
    {
        next_token();
        expr->kind = EXPR::BOOL_LIT;
        expr->lit_val = 1;
    } break;
    default:
    {
        expr->kind = EXPR::UNDEFINED;
    }
    }
    return expr;
}

static Expression *parse_expr_clause() {
    Expression *expr;
    if (match_token(TOK::NOT)) {
        expr = new Expression;
        expr->loc = loc;
        expr->kind = EXPR::NOT;
        expr->e1 = parse_expr_clause();
    } else {
        expr = parse_expr_primary();
    }
    return expr;
}

static Buf<Expression*> parse_expr_list() {
    Buf<Expression*> expr_list;
    if (is_token(TOK::RPAR)) {
        return expr_list;
    }
    while (true) {
        expr_list.push(parse_expr());
        if (!is_token(TOK::RPAR)) {
            if (!match_token(TOK::COMMA)) {
                syntax_error("Expected `,` in expression list.");
                break;
            }
        } else {
            break;
        }
    }
    return expr_list;
}

static Expression *parse_expr() {
    Expression *e1 = parse_expr_clause();
    assert(e1);
    if (!is_op(token.kind)) {
        // Handle ArrayLength and MessageSend.
        if (match_token(TOK::DOT)) {
            Expression *expr;
            if (is_token(TOK::ID)) {
                const char *id = token.id;
                next_token();
                if(id == length_) {  // ArrayLength
                    expr = new Expression;
                    expr->loc = loc;
                    expr->kind = EXPR::ARR_LEN;
                    expr->e1 = e1;
                } else {  // MessageSend
                    BinaryExpression *be = new BinaryExpression;
                    be->loc = loc;
                    if (!match_token(TOK::LPAR)) {
                        // Maybe don't advance here because of expect_token* ?
                        expect_token_in_rule(TOK::LPAR, "message send expression");
                        be->kind = EXPR::UNDEFINED;
                    } else {
                        be->kind = EXPR::MSG_SEND;
                        be->e1 = e1;
                        be->msd = new MessageSendData;
                        be->msd->loc = loc;
                        be->msd->id = id;
                        be->msd->expr_list = parse_expr_list();
                        expect_token_in_rule(TOK::RPAR, "message send expression");
                    }
                    expr = be;
                }
            } else {  // Error
                expr = new Expression;
                expr->loc = loc;
                expr->kind = EXPR::UNDEFINED;
            }
            return expr;
        }
        // Simple Primary Expression (or Clause)
        return e1;
    }
    TOK kind;
    BinaryExpression *bin_expr;
    // Note: This is _not_ a loop; We can't have 1 + 2 + 3.
    if (is_op(token.kind)) {
        if(e1->kind == EXPR::NOT && !is_token(TOK::AND_AND)) {
            syntax_error("Can't use a `not expression` as operand in operator ",
                         token_name(token.kind));
            return e1;
        }
        bin_expr = new BinaryExpression;
        bin_expr->loc = loc;
        bin_expr->e1 = e1;
        kind = token.kind;
        next_token();
        if (kind == TOK::AND_AND) {
            // AndExpression is the only one that can have as operands
            // `Clause`s.
            bin_expr->kind = EXPR::AND;
            bin_expr->e2 = parse_expr_clause();
            assert(bin_expr->e2);
        } else {
            Expression *e2 = parse_expr_primary();
            assert(e2);
            bin_expr->e2 = e2;
            if (e2->kind == EXPR::UNDEFINED) {
                // No type-checking here! We don't start check what the expression is!
                // But, since we got an undefined, we can issue a better message than
                // "expected expression".
                syntax_error("Expected integer expression as a right-hand-side "
                             "expression for operator `", token_name(kind), "`.");
            }
            switch (kind) {
            case TOK::LT:       bin_expr->kind = EXPR::CMP;       break;
            case TOK::PLUS:     bin_expr->kind = EXPR::PLUS;      break;
            case TOK::MINUS:    bin_expr->kind = EXPR::MINUS;     break;
            case TOK::STAR:     bin_expr->kind = EXPR::TIMES;     break;
            case TOK::LBRACKET:
            {
                bin_expr->kind = EXPR::ARR_LOOK;
                expect_token_in_rule(TOK::RBRACKET, "array lookup expression");
            } break;
            default: assert(0);
            }
        }
    }
    return bin_expr;
}

/* Statements
 */
static bool starts_statement(TOK kind) {
    return TOK::__FIRST_STMT <= kind && kind <= TOK::__LAST_STMT;
}

template<typename T>
static void parse_stmt_and_push(Buf<T> *stmts) {
    Statement *s = parse_stmt();
    if (s->kind != STMT::UNDEFINED) {
        stmts->push(s);
    }
}

static Buf<Statement*> parse_stmt_block() {
    Buf<Statement*> stmts;
    while (!is_token(TOK::RBRACE)) {
        parse_stmt_and_push(&stmts);
    }
    return stmts;
}

// TODO: In skip_tokens(), it may be a better choice to skip for newline
// and not semicolon (that requires change in design as newline is not a token).
// Also, skip_tokens() should possibly accept an array instead of a single token.
static Statement *parse_stmt() {
    // The contract is that if the caller called this function,
    // then a statement _must_ follow. In other words, the caller
    // is responsible to check whether a statement follows.
    // The caller is also responsible for checking whether
    // an UndefinedStatement was returned.
    if (!starts_statement(token.kind)) {
        syntax_error("Expected `{`, `Identifier`, `if`, `while`"
                     " or `System.out.println` to start a statement, found:",
                     " `", token, "`");
        Statement *s = new UndefinedStatement;
        s->loc = loc;
        return s;
    }

    Statement *s = NULL;
    switch (token.kind) {
    case TOK::LBRACE:
    {
        next_token();
        BlockStatement *b = new BlockStatement;
        b->loc = loc;
        b->block = parse_stmt_block();
        expect_token(TOK::RBRACE);
        s = b;
    } break;
    case TOK::ID:
    {
        next_token();
        const char *id = token.id;
        Statement *s2 = NULL;
        if (match_token(TOK::ASGN)) {
            AssignmentStatement *asgn_stmt = new AssignmentStatement;
            asgn_stmt->loc = loc;
            asgn_stmt->id = id;
            asgn_stmt->rhs = parse_expr();
            if (asgn_stmt->rhs->kind == EXPR::UNDEFINED) {
                syntax_error_no_ln("RHS expression of assignment to ", id, " is invalid");
                skip_tokens(TOK::SEMI);
                goto Lerror;
            }
            expect_token_in_rule(TOK::SEMI, "assignment statement");
            s2 = asgn_stmt;
        } else if (match_token(TOK::LBRACKET)) {
            ArrayAssignmentStatement *arr_as = new ArrayAssignmentStatement;
            arr_as->loc = loc;
            arr_as->id = id;
            arr_as->index = parse_expr();
            expect_token_in_rule(TOK::RBRACKET, "array assignment statement");
            if (!expect_token_in_rule(TOK::ASGN, "array assignment statement")) {
                goto Lerror;
            }
            arr_as->rhs = parse_expr();
            if (arr_as->rhs->kind == EXPR::UNDEFINED) {
                syntax_error_no_ln("RHS expression of array assignment to ", id, " is invalid");
                skip_tokens(TOK::SEMI);
                goto Lerror;
            }
            expect_token_in_rule(TOK::SEMI, "array assignment statement");
            s2 = arr_as;
        } else {
            // Make a poinsoned statement
            syntax_error("Expected eiter `=` or `[` following id: ", id,
                         " in assignment statement");
            goto Lerror;
        }
        s = s2;
    } break;
    case TOK::IF:
    {
        next_token();
        IfStatement *if_stmt = new IfStatement;
        if_stmt->loc = loc;
        expect_token_in_rule(TOK::LPAR, "if statement");
        if_stmt->cond = parse_expr();
        if (if_stmt->cond->kind == EXPR::UNDEFINED) {
            syntax_error_no_ln("Condition expression of `if` is invalid");
            skip_tokens(TOK::RPAR);
            goto Lerror;
        }
        expect_token_in_rule(TOK::RPAR, "if statement");
        if_stmt->then = parse_stmt();
        if (!expect_token_in_rule(TOK::ELSE, "if statement")) {
            goto Lerror;
        } 
        if_stmt->else_= parse_stmt();
        s = if_stmt;
    } break;
    case TOK::WHILE:
    {
        next_token();
        WhileStatement *while_stmt = new WhileStatement;
        while_stmt->loc = loc;
        expect_token_in_rule(TOK::LPAR, "while statement");
        while_stmt->cond = parse_expr();
        if (while_stmt->cond->kind == EXPR::UNDEFINED) {
            syntax_error_no_ln("Condition expression of `while` is invalid");
            skip_tokens(TOK::RPAR);
            goto Lerror;
        }
        expect_token_in_rule(TOK::RPAR, "while statement");
        while_stmt->body = parse_stmt();
        s = while_stmt;
    } break;
    case TOK::PRINT:
    {
        next_token();
        PrintStatement *prnt = new PrintStatement;
        prnt->loc = loc;
        expect_token_in_rule(TOK::LPAR, "System.out.println");
        prnt->to_print = parse_expr();
        if (prnt->to_print->kind == EXPR::UNDEFINED) {
            syntax_error_no_ln("Expression inside System.out.println is invalid");
            skip_tokens(TOK::SEMI);
            goto Lerror;
        }
        expect_token_in_rule(TOK::RPAR, "System.out.println");
        expect_token_in_rule(TOK::SEMI, "System.out.println");
        s = prnt;
    } break;
    default: assert(0);
    }
    assert(s);
    return s;
Lerror:
    s = new UndefinedStatement;
    s->loc = loc;
    return s;
}

/* Types (specifiers)
 */
static bool starts_type(TOK kind) {
    return kind == TOK::INT || kind == TOK::BOOLEAN || kind == TOK::ID;
}

static Typespec parse_typespec() {
    Typespec tspec;
    tspec.kind = TYSPEC::UNDEFINED;
    // The contract is that if the caller called this function,
    // then a type _must_ follow. In other words, the caller
    // is responsible to check whether a type follows.
    // The caller is also responsible for checking whether
    // an TYSPEC::UNDEFINED was returned.
    if (!starts_type(token.kind)) {
        syntax_error("Expected start of type specifier"
                     " (`int` or `boolean`), found ", token);
        return tspec;
    }
    switch (token.kind) {
    case TOK::INT:
    {
        next_token();
        if (match_token(TOK::LBRACKET)) {
            expect_token_in_rule(TOK::RBRACKET, "int array type specifier");
            tspec.kind = TYSPEC::ARR;
        } else {
            tspec.kind = TYSPEC::INT;
        }
    } break;
    case TOK::BOOLEAN:
    {
        next_token();
        tspec.kind = TYSPEC::BOOL;
        if (is_token(TOK::LBRACKET)) {
            warning("You can't have arrays of `boolean` type, only of `int`.");
        }
    } break;
    case TOK::ID:
    {
        tspec.id = token.id;
        tspec.kind = TYSPEC::ID;
        next_token();
    } break;
    default: tspec.kind = TYSPEC::UNDEFINED;
    }
    // The caller is responsible for recognizing
    // an UNDEFINED type specifier and issuing
    // a message.
    return tspec;
}

/* Declarations
 */
static bool starts_local() {
    if (!starts_type(token.kind)) {
        return false;
    } else if (is_token(TOK::ID)) {
        // A little bit of hackiness:
        // An identifer can start both a statement
        // and a type. So, you have to peek a token
        // and see. The logic of peeking
        // is hidden in the lexer.
        token_t save_token = peek_token();
        if (save_token.kind == TOK::ASGN ||
            save_token.kind == TOK::LBRACKET)
        {
            return false;
        }
        return true;
    } else {
        return true;
    }
}

static LocalDeclaration *parse_local() {
    LocalDeclaration *local = new LocalDeclaration;
    local->loc = loc;
    if (!starts_local()) {
        syntax_error("Expected start of type specifier"
                     " (`int` or `boolean`), to start local declaration",
                     " (either parameter or variable), found `", token, "`");
        goto Lerror;
    }
    local->typespec = parse_typespec();
    if (local->typespec.kind == TYSPEC::UNDEFINED) {
        goto Lerror;
    }
    local->id = token.id;
    if (!match_token(TOK::ID)) {
        syntax_error_no_ln("Expected Identifier after type specifier `");
        typespec_print(local->typespec);
        printf("`");
        log(", found `", token, "`\n");
        goto Lerror;
    }
    return local;
Lerror:
    local->make_undefined();
    return local;
}

template<typename T>
static void parse_local_and_push(Buf<T> *locals) {
    LocalDeclaration *local = parse_local();
    if (!local->is_undefined()) {
        locals->push(local);
    }
}

static MainClass parse_main_class() {
    MainClass main_class;
    // Note: Most gymnastics are for error reporting.
    // TODO: Some errors below are probably fatal, so we should
    // stop there.
    if (match_token(TOK::CLASS)) {  // "class"
        main_class.id = token.id;
        if (match_token(TOK::ID)) {  // Identifier
            expect_token_in_rule(TOK::LBRACE, "MainClass declaration");    // "{"
            expect_token_in_rule(TOK::PUBLIC, "MainClass declaration");    // "public"
            expect_token_in_rule(TOK::STATIC, "MainClass declaration");    // "static"
            expect_token_in_rule(TOK::VOID, "MainClass declaration");      // "void"
            if (!is_token(TOK::ID) || token.id != main_) {
                syntax_error("Expected identifier `main` in MainClass declaration to"
                             " start the declaration of the main method.");
                goto Lerror;
            }
            // Advance anyway
            next_token();
            expect_token_in_rule(TOK::LPAR, "`main` method declaration");      // "("
            if (!match_token(TOK::STRING)) {
                syntax_error("`main` method of MainClass can only have one"
                             " argument, of String[] type.");
                // Advance anyway
                next_token();
            }
            expect_token_in_rule(TOK::LBRACKET, "MainClass declaration");  // "["
            expect_token_in_rule(TOK::RBRACKET, "MainClass declaration");  // "]"
            expect_token_in_rule(TOK::ID, "MainClass declaration");        // "Identifier"
            if (is_token(TOK::COMMA)) {
                syntax_error_no_ln("`main` method of MainClass can only have one"
                             " argument, of String[] type.");
                skip_tokens(TOK::RPAR);
            } else {
                // ")"
                expect_token_in_rule(TOK::RPAR, "`main` method of MainClass declaration");
            }
            // "{"
            expect_token_in_rule(TOK::LBRACE, "`main` method of MainClass declaration");
            // Parse VarDeclarations
            while (starts_local()) {
                parse_local_and_push(&main_class.vars);
                expect_token_in_rule(TOK::SEMI, "var declaration");
            }
            while (starts_statement(token.kind)) {
                parse_stmt_and_push(&main_class.stmts);
            }
            // "}" - close main
            expect_token_in_rule(TOK::RBRACE, "`main` method of MainClass declaration");
            expect_token_in_rule(TOK::RBRACE, "MainClass declaration");  // "}" - close class
        } else {
            syntax_error("MainClass name was expected, but `", token.kind,
                         "` was encountered.");
            goto Lerror;
        }
    } else {
        syntax_error("`class` keyword was expected, in order to start the "
                     "MainClass. But `", token.kind, "` was encountered.");
        goto Lerror;
    }
    return main_class;
Lerror:
    main_class.make_undefined();
    return main_class;
}

static bool starts_method(TOK kind) {
    return kind == TOK::PUBLIC;
}

static MethodDeclaration *parse_method() {
    MethodDeclaration *method_decl = new MethodDeclaration;
    method_decl->loc = loc;
    Typespec tyspec;
    tyspec.kind = TYSPEC::UNDEFINED;
    Expression *expr = NULL;
    if (!match_token(TOK::PUBLIC)) {
        syntax_error("Expected start of type specifier"
                     " (`int` or `boolean`), found ", token);
        goto Lerror;
    }
    tyspec = parse_typespec();
    if (tyspec.kind == TYSPEC::UNDEFINED) {
        goto Lerror;
    }
    method_decl->id = token.id;
    if (!expect_token_in_rule(TOK::ID, "method declaration")) {
        goto Lerror;
    }
    if (!match_token(TOK::LPAR)) {
        syntax_error("Expected `(` before parameter list in method declaration with id: ",
                     method_decl->id);
    }
    // Parse FormalParameters
    while (starts_local()) {
        parse_local_and_push(&method_decl->params);
        match_token(TOK::COMMA);
    }
    if (!match_token(TOK::RPAR)) {
        syntax_error("Expected `)` after parameter list in method declaration with id: ",
                     method_decl->id);
    }
    if (!match_token(TOK::LBRACE)) {
        syntax_error("Expected `{` list in method declaration with id: ",
                     method_decl->id);
    }
    // Parse VarDeclarations
    while (starts_local()) {
        parse_local_and_push(&method_decl->vars);
        expect_token_in_rule(TOK::SEMI, "var declaration");
    }
    while (starts_statement(token.kind)) {
        parse_stmt_and_push(&method_decl->stmts);
    }
    if (!match_token(TOK::RETURN)) {
        syntax_error("Expected `return` in the end of method with id: ",
                     method_decl->id);
    }
    expr = parse_expr();
    if (expr->kind == EXPR::UNDEFINED) {
        syntax_error("Invalid return expression in method with id: ",
                     method_decl->id);
    }
    method_decl->ret = expr;
    if (!match_token(TOK::SEMI)) {
        syntax_error("Expected `;` after `return` expression of"
                     " method declaration with id: ", method_decl->id,
                     " found `", token, "`");
    }
    if (!match_token(TOK::RBRACE)) {
        syntax_error("Expected `}` in the end of method with id: ",
                     method_decl->id, ", found `", token, "`");
    }
    return method_decl;
Lerror:
    method_decl->make_undefined();
    return method_decl;
}

template<typename T>
static void parse_method_and_push(Buf<T> *methods) {
    MethodDeclaration *m = parse_method();
    if (!m->is_undefined()) {
        methods->push(m);
    }
}

static TypeDeclaration *parse_type_declaration() {
    TypeDeclaration *cls = new TypeDeclaration;
    cls->loc = loc;
    const char *id = NULL;
    if (!match_token(TOK::CLASS)) {
        syntax_error("Expected `class` to start TypeDeclaration");
        goto Lerror;
    }
    id = token.id;
    if (!expect_token_in_rule(TOK::ID, "class declaration")) {
        goto Lerror;
    }
    cls->id = id;
    if (match_token(TOK::EXTENDS)) {
        cls->extends = token.id;
        if (!expect_token_in_rule(TOK::ID, "class extends declaration")) {
            goto Lerror;
        }
    }
    if (!match_token(TOK::LBRACE)) {
        syntax_error("Expected `{` in class declaration with name: ", cls->id);
    }
    // Parse VarDeclarations
    while (starts_local()) {
        parse_local_and_push(&cls->vars);
        expect_token_in_rule(TOK::SEMI, "var declaration");
    }
    while (starts_method(token.kind)) {
        parse_method_and_push(&cls->methods);
    }
    if (!match_token(TOK::RBRACE)) {
        syntax_error("Expected `}` in class declaration with name: ", cls->id);
    }
    return cls;
Lerror:
    cls->make_undefined();
    return cls;
}

Goal parse_goal() {
    Goal goal;
    goal.main_class = parse_main_class();
    if (goal.main_class.is_undefined()) {
        fatal_error("Main Class had errors that can't be recovered.");
    }
    while (is_token(TOK::CLASS)) {
        TypeDeclaration *type_decl = parse_type_declaration();
        if (!type_decl->is_undefined()) {
            goal.type_decls.push(type_decl);
        }
    }
    return goal;
}

void parse_init(const char *file_contents, const char *filename) {
    lexer_init(file_contents, filename);
    // TODO: Move this to some general initialization.
    EOI_has_been_found = false;
    main_ = str_intern("main");
    length_ = str_intern("length");
    set_indent_char('-');
}
