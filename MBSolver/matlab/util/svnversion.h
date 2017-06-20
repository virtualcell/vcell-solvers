#ifndef svnversion_h
#define svnversion_h
#if !defined(SVNVERSION)
#error SVNVERSION version not defined
#endif
#define VCELLSVNQ(x) #x
#define VCELLSVNQUOTE(x) VCELLSVNQ(x)
//#define SVN_VERSION_TAG static const char * const svn_version_string = __FILE__"SVN_version:"VSTR(SVNVERSION) ;
#define SVN_VERSION_TAG static const char * const svn_version_string = __FILE__ " " __DATE__ "SVN_version:  " VCELLSVNQUOTE(SVNVERSION); 
#endif
