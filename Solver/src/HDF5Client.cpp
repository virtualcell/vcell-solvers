#include <HDF5Client.h>

/**
* declare static function of ResultPoint. Can't be a static field because
* H5:PredType constants may have been initialized yet when fields initialized due to
* order of initialization of static fields
* see http://stackoverflow.com/questions/1005685/c-static-initialization-order
*/
using vcellH5::VarLen;
using moving_boundary::ResultPoint; 
using moving_boundary::PODPoint;
template <class T>
VarLen<PODPoint<T> > &  PODPoint<T>::vectorType( ) {
	static VarLen<PODPoint<double> > instance(PODPoint<double>::getType( ));
	return instance;
}

template struct moving_boundary::PODPoint<double>;
