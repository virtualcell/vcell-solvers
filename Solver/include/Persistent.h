#ifndef Persistent_h
#define Persistent_h
namespace std {
	class type_info;
}

namespace vcell_persist {

	/**
	* register type_info name for use as token in file
	*/
	void registerTypeToken(const std::type_info &, const char * token); 
	//in persist.cpp
}

#endif
