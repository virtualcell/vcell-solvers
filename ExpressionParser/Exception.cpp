#include "Exception.h"
#include "ParserException.h"
#include "RuntimeException.h"
#include "FunctionDomainException.h"
#include "FunctionRangeException.h"
#include "ExpressionException.h"
#include "DivideByZeroException.h"
#include "ParseException.h"
#include "IOException.h"
#include "ExpressionBindingException.h"
#include <stdlib.h>

VCell::Exception::Exception(string titleVal, string msg)
{
	title = titleVal;
	message = msg;
}

VCell::Exception::Exception(string msg)
{
	title = "Exception";
	message = msg;
}

VCell::Exception::~Exception(void)
{
}

string VCell::Exception::getExactMessage() {
	return message;
}

string VCell::Exception::getMessage(void)
{
	return title + " : " + getExactMessage();
}

void VCell::Exception::replaceMessage(string& replacementMessage)
{
	message = replacementMessage;
}

void VCell::Exception::rethrowException(Exception& ex, string replacementMessage)
{	
	string message = ex.getExactMessage();
	if (replacementMessage.size()>0){
		ex.replaceMessage(replacementMessage);
	}

	if (typeid(ex) == typeid(ParseException)) {
		ParseException* castex = dynamic_cast<ParseException*>((Exception*)(&ex));
		throw (*castex);
	}

	if (typeid(ex) == typeid(ParserException)) {
		ParserException* castex = dynamic_cast<ParserException*>((Exception*)(&ex));
		throw (*castex);
	}

	if (typeid(ex) == typeid(DivideByZeroException)) {
		DivideByZeroException* castex = dynamic_cast<DivideByZeroException*>((Exception*)(&ex));
		throw (*castex);
	}

	if (typeid(ex) == typeid(FunctionDomainException)) {
		FunctionDomainException* castex = dynamic_cast<FunctionDomainException*>((Exception*)(&ex));
		throw (*castex);
	}

	if (typeid(ex) == typeid(FunctionRangeException)) {
		FunctionRangeException* castex = dynamic_cast<FunctionRangeException*>((Exception*)(&ex));
		throw (*castex);
	}

	if (typeid(ex) == typeid(ExpressionBindingException)) {
		ExpressionBindingException* castex = dynamic_cast<ExpressionBindingException*>((Exception*)(&ex));
		throw (*castex);
	}

	if (typeid(ex) == typeid(ExpressionException)) {
		ExpressionException* castex = dynamic_cast<ExpressionException*>((Exception*)(&ex));
		throw (*castex);
	}

	if (typeid(ex) == typeid(RuntimeException)) {
		RuntimeException* castex = dynamic_cast<RuntimeException*>((Exception*)(&ex));
		throw (*castex);
	}

	throw ex;
}

#if ( !defined(WIN32) && !defined(WIN64) )
char* itoa( int value, char* result, int base ) {	
	
	if (base < 2 || base > 16) { 
		*result = 0; 
		return result; 
	}
	
	char* out = result;	
	int quotient = value;	
	do {	
		*out = "0123456789abcdef"[ std::abs( quotient % base ) ];	
		++out;	
		quotient /= base;	
	} while ( quotient);	
	
	// Only apply negative sign for base 10	
	if ( value < 0 && base == 10) 
		*out++ = '-';
	
	std::reverse(result, out );	
	*out = 0;	
	return result;	
}
#endif

string VCell::Exception::add_escapes(string str)
{
	string retval;
    char ch;
    for (unsigned int i = 0; i < str.length(); i++) {
        switch (str[i]) {
            case 0 :
				retval += "";
                continue;
            case '\b' :
                retval += "\\b";
                continue;
            case '\t' :
                retval += "\\t";
                continue;
            case '\n' :
                retval += "\\n";
                continue;
            case '\f' :
                retval += "\\f";
                continue;
            case '\r' :
                retval += "\\r";
                continue;
            case '\"' :
                retval += "\\\"";
                continue;
            case '\'' :
                retval += "\\\'";
                continue;
            case '\\' :
                retval += "\\\\";
                continue;
            default :
                if ((ch = str[i]) < 0x20 || ch > 0x7e) {
					char chrs[20];
					memset(chrs, 0, 20 * sizeof(char));
					itoa(ch, chrs, 16);
                    string s = string("0000") + chrs;
                    retval += "\\u" + s.substr(s.length() - 4, 4);
                } else {
                    retval += ch;
                }
                continue;
        }
    }
    return retval;
}
