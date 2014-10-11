#ifndef Persistent_h
#define Persistent_h
namespace std {
	class type_info;
}

namespace vcell_persist {
	/**
	* standard interface for binarily persistent objects
	* typically, restoration is via constructor, so no function is provided for
	* restoration
	*/
	struct Persistent {
		/**
		* store object to stream
		*/
		virtual void persist(std::ostream &) = 0;
	};

	/**
	* register type_info name for use as token in file
	*/
	void registerTypeToken(const std::type_info &, const char * token); 
	//in persist.cpp
}

#endif
