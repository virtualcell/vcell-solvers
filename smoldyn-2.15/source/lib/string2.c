/* Steven Andrews, 1/99
See documentation called string2_doc.doc
Copyright 2003-2007 by Steven Andrews.  This work is distributed under the terms
of the Gnu Lesser General Public License (LGPL). */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "string2.h"


int strisnumber(char *str)	{
 char *ptr;
 int len;

 if(!(len=strlen(str))) return 0;
 strtod(str,&ptr);
 return(ptr==str+len);	}


int okname(char *name)	{
	int ok;

	ok=1;
	ok=ok&&isalpha(*name);
	for(name++;*name&&ok;name++) ok=ok&&(isalnum(*name)||*name=='_');
	return ok; }


char *strrpbrk(char *cs,char *ct) {
	char *s2;
	int i,n;

	if(!cs||!ct) return NULL;
	n=strlen(ct);
	for(s2=cs+strlen(cs)-1;s2>=cs;s2--)
		for(i=0;i<n;i++)
			if(*s2==ct[i]) return s2;
	return NULL; }


char *StringCopy(char *s) {
	char *s2;
	int i;

	s2=(char *) calloc(strlen(s)+1,sizeof(char));
	if(!s2) return 0;
	for(i=0;s[i];i++)	s2[i]=s[i];
	s2[i]='\0';
	return s2; }


unsigned char *PascalString(char *s) {
	unsigned char *s2;
	int i;

	s2=(unsigned char *) calloc(strlen(s)+1,sizeof(unsigned char));
	if(!s2) return 0;
	for(i=0;s[i];i++)	s2[i+1]=s[i];
	s2[0]=(unsigned char) i;
	return s2; }


char *EmptyString() {
	char *s;
	int i;

	s=(char*) calloc(STRCHAR,sizeof(char));
	if(s)	for(i=0;i<STRCHAR;i++)	s[i]='\0';
	return s; }


char *StrChrQuote(char *cs,char c) {
	int qt;

	qt=0;
	for(;*cs;cs++)	{
		if(*cs=='"')	qt=!qt;
		else if(*cs==c&&!qt) return cs;	}
	return NULL; }


int StrChrPQuote(char *cs,char c) {
	int i,pn,qt;

	pn=qt=0;
	for(i=0;i<strlen(cs);i++)
		if(cs[i]=='"') qt=!qt;
		else if(cs[i]==c&&!qt&&!pn) return i;
		else if(cs[i]=='('&&!qt) pn++;
		else if(cs[i]==')'&&!qt&&pn) pn--;
		else if(cs[i]==')'&&!qt) return -2;
	if(pn) return -2;
	if(qt) return -3;
	return -1; }


int StrrChrPQuote(char *cs,char c) {
	int i,pn,qt;

	pn=qt=0;
	for(i=strlen(cs)-1;i>=0;i--)
		if(cs[i]=='"') qt=!qt;
		else if(cs[i]==c&&!qt&&!pn) return i;
		else if(cs[i]==')'&&!qt) pn++;
		else if(cs[i]=='('&&!qt&&pn) pn--;
		else if(cs[i]=='('&&!qt) return -2;
	if(pn) return -2;
	if(qt) return -3;
	return -1; }


int strreadni(char *s,int n,int *a,char **endp) {
	int i,ok;
	char *s2;

	s2=s;
	ok=1;
	for(i=0;i<n&&ok;i++) {
		a[i]=(int) strtol(s,&s2,10);
		if(s2==s) ok=0;
		s=s2; }
	if(endp) *endp=s2;
	return ok?i:i-1; }


int strreadnf(char *s,int n,float *a,char **endp) {
	int i,ok;
	char *s2;

	s2=s;
	ok=1;
	for(i=0;i<n&&ok;i++) {
		a[i]=(float) strtod(s,&s2);
		if(s2==s) ok=0;
		s=s2; }
	if(endp) *endp=s2;
	return ok?i:i-1; }


int strreadnd(char *s,int n,double *a,char **endp) {
	int i,ok;
	char *s2;

	s2=s;
	ok=1;
	for(i=0;i<n&&ok;i++) {
		a[i]=strtod(s,&s2);
		if(s2==s) ok=0;
		s=s2; }
	if(endp) *endp=s2;
	return ok?i:i-1; }


int strreadns(char *s,int n,char **a,char **endp) {
	int i,j;
	char *s2;

	s2=s;
	j=1;
	for(i=0;i<n&&*s2;i++) {
		while(isspace(*s2)) s2++;
		for(j=0;!isspace(*s2)&&*s2;j++) a[i][j]=*(s2++);
		if(j) a[i][j]='\0'; }
	if(endp) *endp=s2;
	return j?i:i-1; }


char *strnword(char *s,int n) {
	char *s2;

	if(!s) return NULL;
	s2=s;
	if(!isspace(*s)) n--;
	for(;n>0&&*s2;n--) {
		while(!isspace(*s2)&&*s2) s2++;
		while(isspace(*s2)) s2++; }
	return *s2?s2:NULL; }


char *strnword1(char *s,int n) {
	char *s2;

	if(!s) return NULL;
	s2=s;
	for(n--;n>0&&*s2;n--) {
		while(*s2&&*s2!=' '&&*s2!='\t') s2++;
		if(*s2) s2++; }
	if(*s2==' '||*s2=='\t'||!*s2) return NULL;
	else return s2;}


int wordcount(char *s) {
	int n,sp;
	char *s2;

	n=0;
	sp=1;
	for(s2=s;*s2;s2++) {
		if(sp&&!isspace(*s2)) n++;
		sp=isspace(*s2); }
	return n; }


int symbolcount(char *s,char c) {
	int n;

	n=0;
	for(;*s;s++)
		if(*s==c) n++;
	return n; }


int stringfind(char **slist,int n,char *s) {
	int i;

	for(i=0;i<n&&strcmp(slist[i],s);i++);
	return i<n?i:-1; }


int strchrreplace(char *str,char charfrom,char charto) {
	int n;
	char *s1;

	n=0;
	while((s1=strchr(str,charfrom))) {
		*s1=charto;
		n++; }
	return n; }


int strstrreplace(char *str,char *strfrom,char *strto,int max) {
	int n,diff,lento,lenfrom,over;
	char *s1,*s2;
	int i,j;

	if(strto) lento=strlen(strto);
	else lento=0;
	lenfrom=strlen(strfrom);
	diff=lento-lenfrom;
	over=0;
	n=0;
	s2=str;
	while((s1=strstr(s2,strfrom))) {
		i=s1-str;
		if(diff<0) {
			for(j=i+lenfrom;str[j-1]&&j<max;j++)
				str[j+diff]=str[j]; }
		else if(diff>0) {
			for(j=strlen(str);j>=i+lenfrom;j--)
				if(j+diff<max) str[j+diff]=str[j];
					else over=1; }
		for(j=0;j<lento;j++)
			if(i+j<max) str[i+j]=strto[j];
				else over=1;
		if(i+lento<max) s2=s1+lento;
			else s2=str+strlen(str);
		n++; }
	if(over) return -n;
	return n; }


int strbegin(char *strshort,char *strlong,int casesensitive) {
	int i;

	i=0;
	if(casesensitive)
		while(strshort[i]!='\0' && strshort[i]==strlong[i]) i++;
	else
		while(strshort[i]!='\0' && tolower(strshort[i])==tolower(strlong[i])) i++;
	if(strshort[i]=='\0' && i>0) return 1;
	return 0; }


int strbslash2escseq(char *str) {
	char *s1,*s2;

	s1=s2=str;
	while(*s2) {
		if(*s2=='\\') {
			s2++;
			if(*s2=='a') *s1='\a';
			else if(*s2=='b') *s1='\b';
			else if(*s2=='t') *s1='\t';
			else if(*s2=='n') *s1='\n';
			else if(*s2=='v') *s1='\v';
			else if(*s2=='f') *s1='\f';
			else if(*s2=='r') *s1='\r';
			else if(*s2=='\\') *s1='\\';
			else if(*s2=='"') *s1='\"';
			else *s1='\\'; }
		else
			*s1=*s2;
		s1++;
		s2++; }
	*s1=*s2;
	return s2-s1; }


void strcutwhite(char *str,int end) {
	int i,j;

	if(end&2) {	// end of string
		for(i=strlen(str)-1;i>=0&&isspace(str[i]);i--);
		str[i+1]='\0'; }
	if(end&1) {	// start of string
		for(i=0;str[i]!='\0'&&isspace(str[i]);i++);
		j=0;
		for(;str[i]!='\0';i++) str[j++]=str[i];
		str[j]='\0'; }
	return; }
