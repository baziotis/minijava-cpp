#ifndef LEX_H
#define LEX_H

void lexer_init(const char *input, const char *file);
void lexer_test();
void next_token();
token_t peek_token();

#endif
