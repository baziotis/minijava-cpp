#include <stdio.h>
#include "common.h"
#include "tokens.h"

const char* token_name(TOK tok) {
    switch (tok) {
    case TOK::EOI: return "End of Input";
    case TOK::LPAR: return "(";
    case TOK::RPAR: return ")";
    case TOK::LBRACE: return "{";
    case TOK::RBRACE: return "}";
    case TOK::LBRACKET: return "[";
    case TOK::RBRACKET: return "]";
    case TOK::COMMA: return ",";
    case TOK::DOT: return ".";
    case TOK::SEMI: return ";";
    case TOK::INTLIT: return "IntegerLiteral";
    case TOK::ID: return "Identifier";
    case TOK::NOT: return "!";
    case TOK::STAR: return"*";
    case TOK::PLUS: return "+";
    case TOK::MINUS: return "-";
    case TOK::LT: return "<";
    case TOK::AND_AND: return "&&";
    case TOK::ASGN: return "=";

    case TOK::PUBLIC: return "public";
    case TOK::STATIC: return "static";
    case TOK::VOID: return "void";
    case TOK::STRING: return "String";
    case TOK::TRUE: return "true";
    case TOK::FALSE: return "false";
    case TOK::THIS: return "this";
    case TOK::NEW: return "new";
    case TOK::CLASS: return "class";
    case TOK::EXTENDS: return "extewnds";
    
    case TOK::INT: return "int";
    case TOK::BOOLEAN: return "boolean";
    case TOK::ELSE: return "else";
    case TOK::IF: return "if";
    case TOK::PRINT: return "System.out.println";
    case TOK::RETURN: return "return";
    case TOK::WHILE: return "while";
    default: assert(0);
    }
}

/// Given a token, return its precedence.
/// The token must be an operator.
//int op_prec(TOK tok) {

/*
Precedence  |  Associativity  |  Operators

     1         left-to-right      ,

     2         right-to-left      =
                                  += -=
                                  *= /= %=
                                  <<= >>=
                                  &= ^= |=

     3         right-to-left      ?:

     4         left-to-right      ||

     5         left-to-right      &&

     6         left-to-right      |

     7         left-to-right      ^

     8         left-to-right      &

     9         left-to-right      == !=

    10         left-to-right      < > <= >=
    11         left-to-right      << >>
    12         left-to-right      + -
    13         left-to-right      * / %

    // Unary
    14         left-to-right      * & - + ! ~
                                  ++ --
                                  sizeof type-cast
    // Postfix
    15                            ++ --
*/
 
/*
    // Sorted in ascending precedence order.
    // Use TOK_LEN size so that tokens for which
    // precedence is undefined, have 0 value.
    static immutable int[TOK_LEN] prec = [
        // 1: ,
        TOK.COMMA:        1,       // ,

        // 2: =
        TOK.ASSIGN:        2,       // =
        TOK.PLUS_ASSIGN:    2,       // /=
        TOK.MINUS_ASSIGN:    2,       // /=
        TOK.STAR_ASSIGN:    2,       // /=
        TOK.DIV_ASSIGN:    2,       // /=
        TOK.MOD_ASSIGN:    2,       // /=
        TOK.LSHIFT_ASSIGN: 2,       // /=
        TOK.RSHIFT_ASSIGN: 2,       // /=
        TOK.AND_ASSIGN:    2,       // /=
        TOK.XOR_ASSIGN:    2,       // /=
        TOK.OR_ASSIGN:     2,       // /=

        // 4: ||
        TOK.OR_OR:         4,       // ||

        // 5: &&
        TOK.AND_AND:       5,       // &&

        // 6: | 
        TOK.OR:            6,       // |

        // 7: ^
        TOK.XOR:           7,       // ^


        // 8: &
        TOK.AND:           8,       // &

        // 9: == !=
        TOK.EQ:            9,       // ==
        TOK.NEQ:           9,       // !=

        // 10: <= >= < >
        TOK.LEQ:          10,       // <=
        TOK.GEQ:          10,       // >=
        TOK.LT:           10,       // <
        TOK.GT:           10,       // >

        // 11: << >>
        TOK.LSHIFT:       11,       // <<
        TOK.RSHIFT:       11,       // >>

        // 12: + -
        TOK.PLUS:          12,       // +
        TOK.MINUS:          12,       // -

        // 13: * / %
        TOK.STAR:          13,       // *
        TOK.DIV:          13,       // /
        TOK.MOD:          13        // %
    ];

    
    // Note that some tokens don't have the concept of precedence.
    // That's not necessarily an error, so we just return 0
    // which their default value.
    return prec[tok];
}
*/
