#ifndef TOKENS_H
#define TOKENS_H

enum class TOK {
    UNDEFINED,
    EOI,
    // Can start an expression
    INTLIT,
    LPAR,
    NOT,

    // Is a type keyword
    BOOLEAN,
    INT,

    // Can start a statement
    __FIRST_STMT,
    LBRACE = __FIRST_STMT,
    ID,
    IF,
    WHILE,
    PRINT,
    __LAST_STMT = PRINT,

    // TODO: Handle the next couple as just identifiers that we intern ?
    PUBLIC,
    ELSE,
    RETURN,
    STATIC,
    VOID,
    STRING,
    TRUE,
    FALSE,
    THIS,
    NEW,
    CLASS,
    EXTENDS,

    ASGN,
    RPAR,
    RBRACE,
    RBRACKET,
    COMMA,
    DOT,
    SEMI,

    __FIRST_OP,
    AND_AND = __FIRST_OP,
    LT,
    PLUS,
    MINUS,
    STAR,
    LBRACKET,
    __LAST_OP = LBRACKET,

    __LAST = ASGN,
};

constexpr int TOK_LEN = (int)TOK::__LAST - (int)TOK::UNDEFINED;

struct token_t {
    TOK kind;
    // TODO: Eliminate those (with e.g. inheritance
    // or allocating separately manually).
    union {
        const char *id;
        int val;
    };
};


void print_token(token_t tok);
const char* token_name(TOK tok);

#endif
