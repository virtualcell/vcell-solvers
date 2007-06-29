#ifndef PDESOLVERDIANA_H
#define PDESOLVERDIANA_H

#include <VCELL/StructuredPDESolver.h>

#include <vector>
using namespace std;

class SparseMatrix;
class TriDiagMatrix;

typedef struct mixedBoundaryInfo {
	long				index;
	int				mixedBoundaryMask;
} MixedBoundaryInfo ;

typedef vector<MixedBoundaryInfo> MixedBoundaryPoints;


class PdeSolverDiana : public StructuredPDESolver
{
public:
    PdeSolverDiana(VolumeVariable *Var,
        CartesianMesh  *mesh, 
		int             symmflg,
		int             numSolveRegions, 
		int            *solveRegions,
		bool        AbTimeDependent);
    ~PdeSolverDiana(); 

    virtual bool solveEqn(double deltaTime, 
        int volumeIndexStart, int volumeIndexSize, 
        int membraneIndexStart, int membraneIndexSize, bool bFirstTime); 

    double getResidual(double *xsol);
    void      setCurrAxis(Axis axis) { currAxis = axis; }
	long getGlobalIndex(long arrayIndex) {return Gridmap[arrayIndex];}
	long getArraySize() {return arraySize;}

private:
	bool enableRetry;
	void initMatrix(long given_numUnknowns, int additional=0);

protected:
    virtual bool solveEqn1D(bool bFirstTime); 
    virtual bool PCGFormSys(double scale, bool bFirstTime);
    virtual bool PCGFormSys_Reorder(double scale, bool bFirstTime);
    virtual bool PCGFormSymmetricMA(double scale);
    virtual bool PCGFormGeneralMA(double scale);
    virtual bool PCGFormSymmetricMA_Reorder(double scale);
    virtual bool PCGFormGeneralMA_Reorder(double scale);
    //     virtual bool PCGFormMA_Dirichlet();
    virtual bool IsDirichlet(long index);
    virtual int* PCGSolve(bool bFirstTime);
    void RHS_Mixed_Boundary_Update(double RHSscale);
    TriDiagMatrix *pMatx;
    SparseMatrix  *Smat;
    Axis           currAxis;
    double        *u_array;

    // New stuff for PCG Pack
    bool  solveWholeMesh;
    int      symmflg;
    int      NumRegions;
    int      *SolRegions;
    double   GlobalScale;
    MixedBoundaryPoints  mixedBPoints;
    long     *Gridmap;
    long     *Neworder;
    long     arraySize;
};


#endif
