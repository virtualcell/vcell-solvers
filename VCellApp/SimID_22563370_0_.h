//-------------------------
//  SimID_22563370_0_.h
//-------------------------
#ifndef SimID_22563370_0__H
#define SimID_22563370_0__H

#include <VCELL/SimTypes.h>
#include <VCELL/VolumeVarContext.h>
#include <VCELL/MembraneVarContext.h>
#include <VCELL/Simulation.h>
#include <VCELL/VCellModel.h>
#include <VCELL/Feature.h>
#include <VCELL/FastSystem.h>

class CartesianMesh;
class Feature;
class VolumeVariable;
class MembraneVariable;
class ContourVariable;
class VolumeRegion;

const double KMOLE = 0.0016611295681063123;;
const double _F_ = 96480.0;;
const double _K_GHK_ = 1.0E-9;;
const double DEX_Cell_diffusionRate = 20.0;;
const double DEX_EC_init = 1.0;;
const double _T_ = 300.0;;
const double DEX_EC_diffusionRate = 20.0;;
const double _F_nmol_ = 9.648E-5;;
const double DEX_Cell_init = 2.0;;
const double _N_pmol_ = 6.02E11;;
const double K_millivolts_per_volt = 1000.0;;
const double J_flux0 = 2.0;;
const double Voltage_PM = 0.0;;
const double _R_ = 8314.0;;


//---------------------------------------------
//  class UserSimulation
//---------------------------------------------
class UserSimulation : public Simulation
{
 public:
   UserSimulation(CartesianMesh *mesh);
};

//---------------------------------------------
//  class UserVCellModel
//---------------------------------------------
class UserVCellModel : public VCellModel
{
 public:
   UserVCellModel();
};

//---------------------------------------------
//  class Featurecytosol
//---------------------------------------------
class Featurecytosol : public Feature
{
 public:
   Featurecytosol(string& featureName, int priority);
	virtual BoundaryType getXmBoundaryType() { return BOUNDARY_FLUX; }
	virtual BoundaryType getXpBoundaryType() { return BOUNDARY_FLUX; }
	virtual BoundaryType getYmBoundaryType() { return BOUNDARY_FLUX; }
	virtual BoundaryType getYpBoundaryType() { return BOUNDARY_FLUX; }
	virtual BoundaryType getZmBoundaryType() { return BOUNDARY_VALUE; }
	virtual BoundaryType getZpBoundaryType() { return BOUNDARY_VALUE; }

};

//---------------------------------------------
//  class VolumeVarContextcytosolDEX
//---------------------------------------------
class VolumeVarContextcytosolDEX : public VolumeVarContext
{
 public:
    VolumeVarContextcytosolDEX(Feature *feature, string& speciesName);
    virtual bool resolveReferences(Simulation *sim);
 protected:
    virtual double getReactionRate(long volumeIndex);
    virtual void getFlux(MembraneElement *element,double *inFlux, double *outFlux);
 private:
};

//---------------------------------------------
//  class Featureextracellular
//---------------------------------------------
class Featureextracellular : public Feature
{
 public:
   Featureextracellular(string& featureName, int priority);
	virtual BoundaryType getXmBoundaryType() { return BOUNDARY_FLUX; }
	virtual BoundaryType getXpBoundaryType() { return BOUNDARY_FLUX; }
	virtual BoundaryType getYmBoundaryType() { return BOUNDARY_FLUX; }
	virtual BoundaryType getYpBoundaryType() { return BOUNDARY_FLUX; }
	virtual BoundaryType getZmBoundaryType() { return BOUNDARY_VALUE; }
	virtual BoundaryType getZpBoundaryType() { return BOUNDARY_VALUE; }

};

//---------------------------------------------
//  class VolumeVarContextextracellularDEX
//---------------------------------------------
class VolumeVarContextextracellularDEX : public VolumeVarContext
{
 public:
    VolumeVarContextextracellularDEX(Feature *feature, string& speciesName);
    virtual bool resolveReferences(Simulation *sim);
 protected:
    virtual double getReactionRate(long volumeIndex);
    virtual void getFlux(MembraneElement *element,double *inFlux, double *outFlux);
 private:
};


#endif
