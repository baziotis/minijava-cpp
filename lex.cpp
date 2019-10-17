#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>           // printf
#include <cstring>          // strlen()
#include <limits>

#include "common.h"
#include "error.h"          // log()
#include "str_intern.h"     // str_intern_range()
#include "tokens.h"         // TOK, token_t

// Globals
location_t loc;
token_t token;
static const char *input;

static constexpr const char *print_stmt_key = "System.out.println";
static int print_stmt_key_len = strlen(print_stmt_key);

// TODO: That can be moved to a loca in next_token()
static const char *tok_start;

enum { BLANK = 0b1, MIDCHAR = 0b10, DEC = 0b100,
       HEX = 0b1000, OCT = 0b10000, OTHER = 0b100000 };

static uint8_t charmap[256];
static uint16_t digit_from_char[256];

extern Buf<InternedStr> strings;

void scan_int() {
    token.kind = TOK::INTLIT;
    // Find base
    int base = 10;
    if (*input == '0') {
        ++input;
        if (tolower(*input) == 'x') {
            base = 16;
            ++input;
            if (!(charmap[*input]&HEX)) {
                syntax_error("Invalid hex integer literal.");
                return;
            }
        } else if (charmap[*input]&OCT) {
            base = 8;
        } else if (charmap[*input]&(DEC|HEX)) {
            syntax_error("Invalid octal integer literal.");
            token.kind = TOK::INTLIT;
            token.val = 0;
            return;
        }
    }

    // Compute value.
    int val = 0;
    bool overflow = false;
    while (true) {
        int digit = digit_from_char[tolower(*input)];
        if (digit == 0 && *input != '0') {
            break;
        }
        if (digit >= base) {
            syntax_error("Digit `", *input, "` out of range for base ", base);
            break;
        }
        // You can choose to either continue on overflow
        // or eat the remaining digits and set a standard value
        // like 0.
        constexpr int int_max = std::numeric_limits<int>::max();
        if (val > (int_max - digit) / 10) {
            overflow = false;
            warning("Integer literal overflow.");
            // eat remaining digits
            digit = digit_from_char[*input];
            for(;;) {
                if(charmap[*input]&DEC) {
                    ++input;
                } else {
                    digit = digit_from_char[*input];
                    if(digit != 0 && digit < base) {
                        ++input;
                    } else {
                        break;
                    }
                }
            }
            val = int_max;
            break;
        }
        val = val * base + digit;
        ++input;
    }
    token.val = val;
}

void encountered_newline() {
    ++loc.ln;
}

void next_token() {
    tok_start = input;
lex_again:
    if (!input[0]) {
        token.kind = TOK::EOI;
        return;
    }
    // skip whitespace
    while (*input && charmap[*input] & BLANK) {
        if (*input == '\n') {
            encountered_newline();
        }
        input++;
    }
    if (!input[0]) goto lex_again;
    tok_start = input;

    switch (*input) {
    case '.':
    {
        token.kind = TOK::DOT;
        if (charmap[input[1]]&DEC) {
            warning("Floating numbers are not supported.");
        }
        ++input;
    } break;

    case '0' ... '9':
    {
        while (charmap[*input]&DEC) {
            ++input;
        }
        const char *save = input;
        if (*input == '.' || *input == 'e') {
            warning("Floating numbers are not supported.");
            ++save;
        }
        input = tok_start;  // reset input
        scan_int();
        input = save;
    } break;

    // TODO: Maybe an automata-like lexer is not that good these days..
    // Think of making a modified version of memcmp (one that can
    // intelligently be inlined) optimized for small lengths.

    // Explicitly handle letters that start keywords.
    // If we don't match a keyword, we fall-through to the
    // identifier case.
    case 'b':
    {
        if (input[1] == 'o'
        &&  input[2] == 'o'
        &&  input[3] == 'l'
        &&  input[4] == 'e'
        &&  input[5] == 'a'
        &&  input[6] == 'n'
        && !(charmap[input[7]]&(DEC|MIDCHAR))) {
            input += 7;
            token.kind = TOK::BOOLEAN;
            return;
        }
        goto Lid;
    } break;
    case 'c':
    {
        if (input[1] == 'l'
        &&  input[2] == 'a'
        &&  input[3] == 's'
        &&  input[4] == 's'
        && !(charmap[input[5]]&(DEC|MIDCHAR))) {
            input += 5;
            token.kind = TOK::CLASS;
            return;
        }
        goto Lid;
    } break;
    case 'e':
    {
        if (input[1] == 'l'
        &&  input[2] == 's'
        &&  input[3] == 'e'
        && !(charmap[input[4]]&(DEC|MIDCHAR))) {
            input += 4;
            token.kind = TOK::ELSE;
            return;
        }
        if (input[1] == 'x'
        &&  input[2] == 't'
        &&  input[3] == 'e'
        &&  input[4] == 'n'
        &&  input[5] == 'd'
        &&  input[6] == 's'
        && !(charmap[input[7]]&(DEC|MIDCHAR))) {
            input += 7;
            token.kind = TOK::EXTENDS;
            return;
        }
        goto Lid;
    }
    case 'f':
    {
        if (input[1] == 'a'
        &&  input[2] == 'l'
        &&  input[3] == 's'
        &&  input[4] == 'e'
        && !(charmap[input[5]]&(DEC|MIDCHAR))) {
            input += 5;
            token.kind = TOK::FALSE;
            return;
        }
        goto Lid;
    }
	case 'i':
    {
		if (input[1] == 'f'
		&& !(charmap[input[2]]&(DEC|MIDCHAR)))
        {
            input += 2;
            token.kind = TOK::IF;
			return;
		}
		if (input[1] == 'n'
		&&  input[2] == 't'
		&& !(charmap[input[3]]&(DEC|MIDCHAR)))
        {
            input += 3;
            token.kind = TOK::INT;
            return;
		}
		goto Lid;
    }
    case 'n':
    {
		if (input[1] == 'e'
		&&  input[2] == 'w'
		&& !(charmap[input[3]]&(DEC|MIDCHAR))) {
            input += 3;
            token.kind = TOK::NEW;
            return;
        }
        goto Lid;
    } break;
    case 'p':
    {
		if (input[1] == 'u'
		&&  input[2] == 'b'
		&&  input[3] == 'l'
		&&  input[4] == 'i'
		&&  input[5] == 'c'
		&& !(charmap[input[6]]&(DEC|MIDCHAR))) {
            input += 6;
            token.kind = TOK::PUBLIC;
            return;
        }
        goto Lid;
    } break;
    case 'r':
    {
        if (input[1] == 'e'
        &&  input[2] == 't'
        &&  input[3] == 'u'
        &&  input[4] == 'r'
        &&  input[5] == 'n'
        && !(charmap[input[6]]&(DEC|MIDCHAR))) {
            input += 6;
            token.kind = TOK::RETURN;
            return;
        }
        goto Lid;
    }
    case 's':
    {
        if (input[1] == 't'
        &&  input[2] == 'a'
        &&  input[3] == 't'
        &&  input[4] == 'i'
        &&  input[5] == 'c'
        && !(charmap[input[6]]&(DEC|MIDCHAR))) {
            input += 6;
            token.kind = TOK::STATIC;
            return;
        }
        goto Lid;
    }
    case 't':
    {
        if (input[1] == 'r'
        &&  input[2] == 'u'
        &&  input[3] == 'e'
        && !(charmap[input[4]]&(DEC|MIDCHAR))) {
            input += 4;
            token.kind = TOK::TRUE;
            return;
        }
        if (input[1] == 'h'
        &&  input[2] == 'i'
        &&  input[3] == 's'
        && !(charmap[input[4]]&(DEC|MIDCHAR))) {
            input += 4;
            token.kind = TOK::THIS;
            return;
        }
        goto Lid;
    }
    case 'w':
    {
        if (input[1] == 'h'
        &&  input[2] == 'i'
        &&  input[3] == 'l'
        &&  input[4] == 'e'
        && !(charmap[input[5]]&(DEC|MIDCHAR))) {
            input += 5;
            token.kind = TOK::WHILE;
            return;
        }
        goto Lid;
    }
    case 'v':
    {
        if (input[1] == 'o'
        &&  input[2] == 'i'
        &&  input[3] == 'd'
        && !(charmap[input[4]]&(DEC|MIDCHAR))) {
            input += 4;
            token.kind = TOK::VOID;
            return;
        }
        goto Lid;
    }

    case 'S':
    {
        if (input[1] == 't'
        &&  input[2] == 'r'
        &&  input[3] == 'i'
        &&  input[4] == 'n'
        &&  input[5] == 'g'
        && !(charmap[input[6]]&(DEC|MIDCHAR))) {
            input += 6;
            token.kind = TOK::STRING;
            return;
        }
        if (!memcmp(input, print_stmt_key, print_stmt_key_len)) {
            input += print_stmt_key_len;
            token.kind = TOK::PRINT;
            return;
        }
        goto Lid;
    } break;

    // The rest of letters and _ (which correspond to an identifier)
    case 'a': case 'd': case 'h': case 'j':
    case 'k': case 'l': case 'm': case 'o':
    case 'q': case 'u':
    case 'x': case 'y': case 'z':
    case 'A' ... 'R': case 'T' ... 'Z':
    case '_':
    {
Lid:
        while (charmap[*input]&(MIDCHAR|DEC)) {
            ++input;
        }
        token.id = str_intern_range(tok_start, input);
        token.kind = TOK::ID;
    } break;

    case '&':
    {
        if (input[1] == '&') {
            token.kind = TOK::AND_AND;
            input += 2;
        } else {
            warning("There's no `&` token. Maybe you meant to write `&&`.");
            ++input;
        }
    } break;

    case '<': case '\0': case '(': case ')': case '{': case '}':
    case '[': case ']': case ',': case ';': case '!': case '=':
    case '-': case '+': case '*':
    {
        switch (*input)
        {
        case '<': token.kind = TOK::LT; break;
        case '\0': token.kind = TOK::EOI; break;
        case '(': token.kind = TOK::LPAR; break;
        case ')': token.kind = TOK::RPAR; break;
        case '{': token.kind = TOK::LBRACE; break;
        case '}': token.kind = TOK::RBRACE; break;
        case '[': token.kind = TOK::LBRACKET; break;
        case ']': token.kind = TOK::RBRACKET; break;
        case ',': token.kind = TOK::COMMA; break;
        case ';': token.kind = TOK::SEMI; break;
        case '!': token.kind = TOK::NOT; break;
        case '=': token.kind = TOK::ASGN; break;
        case '-': token.kind = TOK::MINUS; break;
        case '+': token.kind = TOK::PLUS; break;
        case '*': token.kind = TOK::STAR; break;
        default: printf("%c\n", *input); assert(0);
        }
        ++input;
    } break;

    case '/':
    {
        ++input;
        if (*input != '/') {
            syntax_error("Expected `/` after `/` so that comment starts;"
                         " skipping the rest of the line");

        }
        while (*input != 0 && *input != '\n') {
            ++input;
        }
        encountered_newline();
        goto lex_again;
    } break;

    default:
        fatal_error("Unrecognized character: ", *input, " - ln: ", loc.ln);
    }
}

token_t peek_token() {
    // Save
    token_t save_token = token;
    location_t save_loc = loc;
    const char *save_input = input;
    next_token();
    // Save the new token to return it.
    token_t new_token = token;
    // Restore
    input = save_input;
    loc = save_loc;
    token = save_token;

    return new_token;
}

void assert_token(TOK kind) {
    //printf("tokens: ");
    //log(kind);
    //printf(" ");
    //log(token.kind);
    //printf("\n");
    assert(token.kind == kind);
    next_token();
}

void assert_token_int(int i) {
    //printf("%d %d\n", token.val, i);
    assert(token.kind == TOK::INTLIT && token.val == i);
    next_token();
}

void assert_token_id(const char *s) {
    assert(token.kind == TOK::ID && token.id == str_intern(s));
    next_token();
}

void assert_token_eof() {
    assert(token.kind == TOK::EOI);
}

void init_input(const char *s) {
    input = s;
    loc.ln = 1;
    next_token();
}

void lexer_init(const char *input, const char *file) {
    int c;
    // Initialize charmap
    for (c = 0; c < 256; ++c) {
        if ('0' <= c && c <= '7') {
            charmap[c] |= OCT;
        }
        if (('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')) {
            charmap[c] |= HEX;
        }
        if ('0' <= c && c <= '9') {
            charmap[c] |= DEC;
            charmap[c] |= HEX;
        }
        
        switch(c) {
            case 0x9 ... 0xc:
            case 0x20:
                charmap[c] |= BLANK;
                break;
            case 0x21 ... 0x2f:
            case 0x3a ... 0x40:
            case 0x5b ... 0x5e:
            case 0x7b ... 0x7e:
                charmap[c] |= OTHER;
                break;
            case 'A' ... 'Z':
            case 'a' ... 'z':
            case '_':
                charmap[c] |= MIDCHAR;
                break;
        }
    }

    // Initialize digit_from_char
    for (c = '0'; c <= '9'; ++c) {
        digit_from_char[c] = c - '0';
    }
    for (c = 'a'; c <= 'f'; ++c) {
        digit_from_char[c] = c - 'a' + 10;
    }

    // Initialize location info
    loc.ln = 1;

    // Initialize the input
    init_input(input);
}

void lexer_test() {
    lexer_init("123 456 0x234", "test.java");
    assert_token_int(123);
    assert_token_int(456);
    assert_token_int(0x234);
    assert_token(TOK::EOI);
    assert_token(TOK::EOI);

    lexer_init("stefanos 456", "test.java");
    assert_token_id("stefanos");
    assert_token_int(456);
    assert_token(TOK::EOI);

    lexer_init("boolean class else if int return while System.out.println", "test.java");
    assert_token(TOK::BOOLEAN);
    assert_token(TOK::CLASS);
    assert_token(TOK::ELSE);
    assert_token(TOK::IF);
    assert_token(TOK::INT);
    assert_token(TOK::RETURN);
    assert_token(TOK::WHILE);
    assert_token(TOK::PRINT);
    assert_token(TOK::EOI);

    lexer_init("&& \0 another", "test.java");
    assert_token(TOK::AND_AND);
    assert_token(TOK::EOI);
    assert_token(TOK::EOI);

    lexer_init("&&  ( ) { } [ ] , ; ! = - + *", "test.java");
    assert_token(TOK::AND_AND);
    assert_token(TOK::LPAR);
    assert_token(TOK::RPAR);
    assert_token(TOK::LBRACE);
    assert_token(TOK::RBRACE);
    assert_token(TOK::LBRACKET);
    assert_token(TOK::RBRACKET);
    assert_token(TOK::COMMA);
    assert_token(TOK::SEMI);
    assert_token(TOK::NOT);
    assert_token(TOK::ASGN);
    assert_token(TOK::MINUS);
    assert_token(TOK::PLUS);
    assert_token(TOK::STAR);
    assert_token(TOK::EOI);

    // Note: No error!
    lexer_init("0r8", "test.java");
    assert_token_int(0);
    assert_token_id("r8");
    assert_token(TOK::EOI);

    // **  Warnings / Errors  ** //

    // Warning: floatings are not supported
    lexer_init(".45", "test.java");
    assert_token(TOK::DOT);
    assert_token_int(45);
    assert_token(TOK::EOI);
    // Syntax Error: Invalid hex constant
    lexer_init("0xw", "test.java");
    // Warning: Integer literal overflow
    lexer_init("0xffffffff", "test.java");
    // Warning: There's no `&` token.
    lexer_init("& +", "test.java");
    // Syntax Error: Invalid octal integer literal.
    lexer_init("085", "test.java");
    // Syntax Error: Digit `8` is out of range for base 8.
    lexer_init("058", "test.java");
}
