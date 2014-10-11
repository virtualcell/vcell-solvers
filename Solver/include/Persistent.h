#ifndef Persistent_h
#define Persistent_h
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
}

#endif
