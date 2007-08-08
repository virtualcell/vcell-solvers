#ifndef EXCEPTION_H
#define EXCEPTION_H

#include "stdinc.h"

namespace VCell {
	class Exception
	{
	public:
		Exception();
		Exception(string message);
		Exception(string title, string message);
		virtual string getExactMessage();
		string getMessage(void);
		~Exception(void);
		static void rethrowException(Exception& ex);
		static string add_escapes(string str);
	//	static string add_escape(char ch);

	protected:
		string message;	
		string title;	
	};
}

#endif
