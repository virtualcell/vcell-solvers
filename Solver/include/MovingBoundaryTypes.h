#ifndef MovingBoundaryTypes_h
#define MovingBoundaryTypes_h
namespace spatial {
	template <class COORD_TYPE,class VALUE_TYPE,int N>
	struct Volume;

}
/**
* implementation type definition
*/
namespace moving_boundary {
	//PRIMARY TYPES
	/**
	* type used for coordinate system of problem implementation
	*/
	typedef long CoordinateType;
	/**
	* type used for distances of problem implementation
	*/
	typedef double DistanceType; 
	/**
	* type used for products/powers of #CoordinateType 
	* necessary to prevent overranging limited value CoordinateTypes
	*/
	typedef double CoordinateProductType; 
	/**
	* type used for #spatial::NormVector
	*/
	typedef double NormalComponent; 
	/**
	* type used for areas/volumes, et. al in problem implementation
	*/
	typedef CoordinateProductType VolumeType;
	/**
	* type used for masses, concentrations, diffusion constants et. al. in problem implementation
	*/
	typedef double BioQuanType;
	/**
	* type for times 
	*/
	typedef double TimeType;
	/**
	* type for velocities 
	*/
	typedef double VelocityType;
	//DERIVATIVE TYPES
	/**
	* typedef for object used to represent Volumes
	*/
	typedef spatial::Volume<CoordinateType,CoordinateProductType,2> Volume2DClass;
}
#endif
