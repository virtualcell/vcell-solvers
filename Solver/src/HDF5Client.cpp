#include <HDF5Client.h>

/**
* declare static member of ResultPoint. Can't be a static member because
* H5:PredType constants may not be initialized 
* see http://stackoverflow.com/questions/1005685/c-static-initialization-order
*/
using vcellH5::VarLen;
using spatial::ResultPoint; 
using spatial::PODPoint;
template <class T>
VarLen<PODPoint<T> > &  PODPoint<T>::vectorType( ) {
	static VarLen<PODPoint<double> > instance(PODPoint<double>::getType( ));
	return instance;
}

template struct spatial::PODPoint<double>;
