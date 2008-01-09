#ifndef SIMPLECHARSTREAM_H
#define SIMPLECHARSTREAM_H

#include "stdinc.h"

class SimpleCharStream
{
protected:
	int bufsize;
	int available;
	int tokenBegin;

private:
	int* bufline;
	int* bufcolumn;
	int column;
	int line;
	boolean prevCharIsCR;
	boolean prevCharIsLF;
	istream* inputStream;
	char* buffer;
	int maxNextCharInd;
	int inBuf;
	int tabSize;
	void ExpandBuff(boolean wrapAround);
	void FillBuff(void);
	void UpdateLineColumn(char c);
	void init(istream* dstream, int startline,  int startcolumn, int buffersize);

public:
	SimpleCharStream(istream* dstream, int startline,  int startcolumn, int buffersize);
	SimpleCharStream(istream* dstream, int startline,  int startcolumn);
	SimpleCharStream(istream* dstream);
	~SimpleCharStream(void);

	static boolean staticFlag;
	int bufpos;

	char BeginToken(void);
	char readChar(void);
	int getColumn(void);
	int getLine(void);
	int getEndColumn(void);
	int getEndLine(void);
	int getBeginColumn(void);
	int getBeginLine(void);
	void backup(int amount);
	string GetImage(void);
	char* GetSuffix(int len);
	void Done(void);
	void adjustBeginLineColumn(int newLine, int newCol);
};

#endif
