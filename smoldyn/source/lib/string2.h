/* Steven Andrews, 11/01.
See documentation called string2_doc.doc
Copyright 2003-2009 by Steven Andrews.  This work is distributed under the terms
of the Gnu Lesser General Public License (LGPL). */

#ifndef __string2_h
#define __string2_h

#define STRCHAR 1024

int strisnumber(char *str);
int okname(char *name);
char *strrpbrk(char *cs,char *ct);
char *StringCopy(char *s);
unsigned char *PascalString(char *s);
char *EmptyString();
char *StrChrQuote(char *cs,char c);
int StrChrPQuote(char *cs,char c);
int StrrChrPQuote(char *cs,char c);
int strreadni(char *s,int n,int *a,char **endp);
int strreadnf(char *s,int n,float *a,char **endp);
int strreadnd(char *s,int n,double *a,char **endp);
int strreadns(char *s,int n,char **a,char **endp);
char *strnword(char *s,int n);
char *strnword1(char *s,int n);
int wordcount(const char *s);
int symbolcount(char *s,char c);
int stringfind(char **slist,int n,const char *s);
int strchrreplace(char *str,char charfrom,char charto);
int strstrreplace(char *str,char *strfrom,char *strto,int max);
int strbegin(char *strshort,char *strlong,int casesensitive);
int strbslash2escseq(char *str);
void strcutwhite(char *str,int end);

#endif
