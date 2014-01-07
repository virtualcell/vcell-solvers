#ifndef DiffuseAdvectCache_h
#define DiffuseAdvectCache_h

namespace spatial {
	/**
	* TODO rename this more generically
	* #MeshElementSpecies needs place to check forward and back diffusion
	* This is interface class to facilitate this without introducing excessive 
	* compile dependencies
	*/
	struct DiffuseAdvectCache {
		virtual ~DiffuseAdvectCache( ) {} 
		virtual void clearDiffuseAdvectCache( ) = 0;
		virtual void checkDiffuseAdvectCache( ) = 0;
	};
}
#endif
