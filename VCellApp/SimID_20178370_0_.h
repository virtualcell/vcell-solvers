//-------------------------
//  SimID_20178370_0_.h
//-------------------------
#ifndef SimID_20178370_0__H
#define SimID_20178370_0__H

#include <VCELL/SimTypes.h>
#include <VCELL/VarContext.h>
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

const double Kr_CalciumCalbindin_BoundCytosol = 8.6;;
const double CaB_C_init = 3.96;;
const double CaBPB_C_init = 47.17;;
const double Kf_CaciumCalbindin_BoundNucleus = 20.0;;
const double CaBP_N_init = 202.83;;
const double _F_ = 96480.0;;
const double Pcalcium = 150.0;;
const double _F_nmol_ = 9.648E-5;;
const double kP = 0.25;;
const double _R_ = 8314.0;;
const double CaBP_C_diffusionRate = 60.0;;
const double _K_GHK_ = 1.0E-9;;
const double Ca_N_diffusionRate = 300.0;;
const double Voltage_PM = 0.0;;
const double J0 = 0.014;;
const double Kr_CalciumBuffer_BoundCytosol = 1.0;;
const double KMOLE = 0.0016611295681063123;;
const double Ca_Rest = 0.1;;
const double CaBP_C_init = 202.83;;
const double Kc = 0.5;;
const double Ca_N_init = 0.1;;
const double Ca_C_diffusionRate = 300.0;;
const double _T_ = 300.0;;
const double Kr_CaciumCalbindin_BoundNucleus = 8.6;;
const double _N_pmol_ = 6.02E11;;
const double Kf_CalciumBuffer_BoundCytosol = 0.1;;
const double Pcalbindin = 30.0;;
const double CaBPB_N_diffusionRate = 60.0;;
const double Pcalbindin_bound = 30.0;;
const double Vmax = -4000.0;;
const double K_millivolts_per_volt = 1000.0;;
const double CaBPB_N_init = 47.17;;
const double Ca_C_init = 0.1;;
const double B_C_init = 396.04;;
const double Voltage_NM = 0.0;;
const double CaBP_N_diffusionRate = 60.0;;
const double Kf_CalciumCalbindin_BoundCytosol = 20.0;;
const double CaBPB_C_diffusionRate = 60.0;;


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
//  class FeatureNucleus
//---------------------------------------------
class FeatureNucleus : public Feature
{
 public:
   FeatureNucleus(char *featureName, int priority);
	virtual BoundaryType getXmBoundaryType() { return BOUNDARY_VALUE; }
	virtual BoundaryType getXpBoundaryType() { return BOUNDARY_VALUE; }
	virtual BoundaryType getYmBoundaryType() { return BOUNDARY_VALUE; }
	virtual BoundaryType getYpBoundaryType() { return BOUNDARY_VALUE; }
	virtual BoundaryType getZmBoundaryType() { return BOUNDARY_VALUE; }
	virtual BoundaryType getZpBoundaryType() { return BOUNDARY_VALUE; }

};

//---------------------------------------------
//  class VolumeVarContextNucleusCaB
//---------------------------------------------
class VolumeVarContextNucleusCaB : public VolumeVarContext
{
 public:
    VolumeVarContextNucleusCaB(Feature *feature, CString speciesName);
    virtual boolean resolveReferences(Simulation *sim);
 protected:
    virtual double getReactionRate(long volumeIndex);
    virtual void getFlux(MembraneElement *element,double *inFlux, double *outFlux);
 private:
};

//---------------------------------------------
//  class VolumeVarContextNucleusCa
//---------------------------------------------
class VolumeVarContextNucleusCa : public VolumeVarContext
{
 public:
    VolumeVarContextNucleusCa(Feature *feature, CString speciesName);
    virtual boolean resolveReferences(Simulation *sim);
 protected:
    virtual double getReactionRate(long volumeIndex);
    virtual void getFlux(MembraneElement *element,double *inFlux, double *outFlux);
 private:
    VolumeVariable      *var_Ca;
};

//---------------------------------------------
//  class VolumeVarContextNucleusCaBP
//---------------------------------------------
class VolumeVarContextNucleusCaBP : public VolumeVarContext
{
 public:
    VolumeVarContextNucleusCaBP(Feature *feature, CString speciesName);
    virtual boolean resolveReferences(Simulation *sim);
 protected:
    virtual double getReactionRate(long volumeIndex);
    virtual void getFlux(MembraneElement *element,double *inFlux, double *outFlux);
 private:
    VolumeVariable      *var_CaBP;
};

//---------------------------------------------
//  class VolumeVarContextNucleusCaBPB
//---------------------------------------------
class VolumeVarContextNucleusCaBPB : public VolumeVarContext
{
 public:
    VolumeVarContextNucleusCaBPB(Feature *feature, CString speciesName);
    virtual boolean resolveReferences(Simulation *sim);
 protected:
    virtual double getReactionRate(long volumeIndex);
    virtual void getFlux(MembraneElement *element,double *inFlux, double *outFlux);
 private:
    VolumeVariable      *var_CaBPB;
};

//---------------------------------------------
//  class FastSystemNucleus
//---------------------------------------------
class FastSystemNucleus : public FastSystem
{
 public:
    FastSystemNucleus();
    virtual boolean resolveReferences(Simulation *sim);
    void initVars();
    void updateDependentVars();
 protected:
    void updateMatrix();
 private:
	Mesh *mesh;
	Simulation *simulation;
   Variable    *var_CaBPB;
   Variable    *var_CaBP;
   Variable    *var_Ca;

   double     __C2;
   double     __C3;
};

//---------------------------------------------
//  class FeatureCytosol
//---------------------------------------------
class FeatureCytosol : public Feature
{
 public:
   FeatureCytosol(char *featureName, int priority);
	virtual BoundaryType getXmBoundaryType() { return BOUNDARY_FLUX; }
	virtual BoundaryType getXpBoundaryType() { return BOUNDARY_FLUX; }
	virtual BoundaryType getYmBoundaryType() { return BOUNDARY_VALUE; }
	virtual BoundaryType getYpBoundaryType() { return BOUNDARY_VALUE; }
	virtual BoundaryType getZmBoundaryType() { return BOUNDARY_VALUE; }
	virtual BoundaryType getZpBoundaryType() { return BOUNDARY_VALUE; }

};

//---------------------------------------------
//  class VolumeVarContextCytosolCaB
//---------------------------------------------
class VolumeVarContextCytosolCaB : public VolumeVarContext
{
 public:
    VolumeVarContextCytosolCaB(Feature *feature, CString speciesName);
    virtual boolean resolveReferences(Simulation *sim);
 protected:
    virtual double getReactionRate(long volumeIndex);
    virtual void getFlux(MembraneElement *element,double *inFlux, double *outFlux);
 private:
};

//---------------------------------------------
//  class VolumeVarContextCytosolCaBPB
//---------------------------------------------
class VolumeVarContextCytosolCaBPB : public VolumeVarContext
{
 public:
    VolumeVarContextCytosolCaBPB(Feature *feature, CString speciesName);
    virtual boolean resolveReferences(Simulation *sim);
 protected:
    virtual double getReactionRate(long volumeIndex);
    virtual void getFlux(MembraneElement *element,double *inFlux, double *outFlux);
 private:
    VolumeVariable      *var_CaBPB;
};

//---------------------------------------------
//  class VolumeVarContextCytosolCa
//---------------------------------------------
class VolumeVarContextCytosolCa : public VolumeVarContext
{
 public:
    VolumeVarContextCytosolCa(Feature *feature, CString speciesName);
    virtual boolean resolveReferences(Simulation *sim);
 protected:
    virtual double getReactionRate(long volumeIndex);
    virtual void getFlux(MembraneElement *element,double *inFlux, double *outFlux);
 private:
    VolumeVariable      *var_Ca;
};

//---------------------------------------------
//  class VolumeVarContextCytosolCaBP
//---------------------------------------------
class VolumeVarContextCytosolCaBP : public VolumeVarContext
{
 public:
    VolumeVarContextCytosolCaBP(Feature *feature, CString speciesName);
    virtual boolean resolveReferences(Simulation *sim);
 protected:
    virtual double getReactionRate(long volumeIndex);
    virtual void getFlux(MembraneElement *element,double *inFlux, double *outFlux);
 private:
    VolumeVariable      *var_CaBP;
};

//---------------------------------------------
//  class FastSystemCytosol
//---------------------------------------------
class FastSystemCytosol : public FastSystem
{
 public:
    FastSystemCytosol();
    virtual boolean resolveReferences(Simulation *sim);
    void initVars();
    void updateDependentVars();
 protected:
    void updateMatrix();
 private:
	Mesh *mesh;
	Simulation *simulation;
   Variable    *var_CaBPB;
   Variable    *var_CaB;
   Variable    *var_Ca;
   Variable    *var_CaBP;

   double     __C0;
   double     __C1;
};

//---------------------------------------------
//  class FeatureExtraCellular
//---------------------------------------------
class FeatureExtraCellular : public Feature
{
 public:
   FeatureExtraCellular(char *featureName, int priority);
	virtual BoundaryType getXmBoundaryType() { return BOUNDARY_FLUX; }
	virtual BoundaryType getXpBoundaryType() { return BOUNDARY_FLUX; }
	virtual BoundaryType getYmBoundaryType() { return BOUNDARY_VALUE; }
	virtual BoundaryType getYpBoundaryType() { return BOUNDARY_VALUE; }
	virtual BoundaryType getZmBoundaryType() { return BOUNDARY_VALUE; }
	virtual BoundaryType getZpBoundaryType() { return BOUNDARY_VALUE; }

};

//---------------------------------------------
//  class VolumeVarContextExtraCellularCaB
//---------------------------------------------
class VolumeVarContextExtraCellularCaB : public VolumeVarContext
{
 public:
    VolumeVarContextExtraCellularCaB(Feature *feature, CString speciesName);
    virtual boolean resolveReferences(Simulation *sim);
 protected:
    virtual double getReactionRate(long volumeIndex);
    virtual void getFlux(MembraneElement *element,double *inFlux, double *outFlux);
 private:
};

//---------------------------------------------
//  class VolumeVarContextExtraCellularCa
//---------------------------------------------
class VolumeVarContextExtraCellularCa : public VolumeVarContext
{
 public:
    VolumeVarContextExtraCellularCa(Feature *feature, CString speciesName);
    virtual boolean resolveReferences(Simulation *sim);
    virtual double getInitialValue(long volumeIndex);
    virtual double getDiffusionRate(long volumeIndex);
 protected:
    virtual double getReactionRate(long volumeIndex);
    virtual void getFlux(MembraneElement *element,double *inFlux, double *outFlux);
 private:
    VolumeVariable      *var_Ca;
};

//---------------------------------------------
//  class VolumeVarContextExtraCellularCaBPB
//---------------------------------------------
class VolumeVarContextExtraCellularCaBPB : public VolumeVarContext
{
 public:
    VolumeVarContextExtraCellularCaBPB(Feature *feature, CString speciesName);
    virtual boolean resolveReferences(Simulation *sim);
 protected:
    virtual double getReactionRate(long volumeIndex);
    virtual void getFlux(MembraneElement *element,double *inFlux, double *outFlux);
 private:
};

//---------------------------------------------
//  class VolumeVarContextExtraCellularCaBP
//---------------------------------------------
class VolumeVarContextExtraCellularCaBP : public VolumeVarContext
{
 public:
    VolumeVarContextExtraCellularCaBP(Feature *feature, CString speciesName);
    virtual boolean resolveReferences(Simulation *sim);
 protected:
    virtual double getReactionRate(long volumeIndex);
    virtual void getFlux(MembraneElement *element,double *inFlux, double *outFlux);
 private:
};


#endif
