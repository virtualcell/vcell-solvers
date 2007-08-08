#ifndef TOKEN_H
#define TOKEN_H

#include "stdinc.h"

class Token
{
public:
	Token(void);
	~Token(void);

	int kind;
	int beginLine, beginColumn, endLine, endColumn;
	string image;

	Token* next;
	Token* specialToken;
	static Token* newToken(int ofKind);
};

#endif
