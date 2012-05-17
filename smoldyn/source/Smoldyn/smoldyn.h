/* Steven Andrews, started 10/22/2001.
 This is the header for the Smoldyn program.  See documentation
 called Smoldyn_doc1.pdf and Smoldyn_doc2.pdf.
 Copyright 2003-2011 by Steven Andrews.  This work is distributed under the terms
 of the Gnu General Public License (GPL). */

#ifndef __smoldyn_h__
#define __smoldyn_h__

#include <time.h>
#include <stdio.h>
#include <string>
using namespace std;
/********************************** General *********************************/

#define DIMMAX 3							// maximum system dimensionality
#define VERYCLOSE 1.0e-12			// distance that's safe from round-off error

enum StructCond {SCinit,SClists,SCparams,SCok};

/********************************* Molecules ********************************/

#define MSMAX 5
#define MSMAX1 6
enum MolecState {MSsoln,MSfront,MSback,MSup,MSdown,MSbsoln,MSall,MSnone,MSsome};
enum MolListType {MLTsystem,MLTport,MLTnone};

typedef struct moleculestruct {
	long int serno;							// serial number
	int list;										// destination list number (ll)
	double *pos;								// dim dimensional vector for position [d]
	double *posx;								// dim dimensional vector for old position [d]
	double *via;								// location of last surface interaction [d]
	double *posoffset;					// position offset arising from jumps [d]
	int ident;									// species of molecule; 0 is empty (i)
	enum MolecState mstate;			// physical state of molecule (ms)
	struct boxstruct *box;			// pointer to box which molecule is in
	struct panelstruct *pnl;		// panel that molecule is bound to if any
	} *moleculeptr;

typedef struct molsuperstruct {
	enum StructCond condition;	// structure condition
	struct simstruct *sim;			// simulation structure
	int maxspecies;							// maximum number of species
	int nspecies;								// number of species, including empty mols.
	char **spname;							// names of molecular species
	double **difc;							// diffusion constants [i][ms]
	double **difstep;						// rms diffusion step [i][ms]
	double ***difm;							// diffusion matrix [i][ms][d]
	double ***drift;						// drift vector [i][ms][d]
	double **display;						// display size of molecule [i][ms] 
	double ***color;						// RGB color vector [i][ms]
	int **exist;								// flag for if molecule could exist [i][ms]
	moleculeptr *dead;					// list of dead molecules [m]
	int maxdlimit;							// maximum allowed size of dead list
	int maxd;										// size of dead molecule list
	int nd;											// total number of molecules in dead list
	int topd;										// index for dead list; above are resurrected
	int maxlist;								// allocated number of live lists
	int nlist;									// number of live lists
	int **listlookup;						// lookup table for live lists [i][ms]
	char **listname;						// names of molecule lists [ll]
	enum MolListType *listtype;	// types of molecule lists [ll]
	moleculeptr **live;					// live molecule lists [ll][m]
	int *maxl;									// size of molecule lists [ll]
	int *nl;										// number of molecules in live lists [ll]
	int *topl;									// live list index; above are reborn [ll]
	int *sortl;									// live list index; above need sorting [ll]
	int *diffuselist;						// 1 if any listed molecs diffuse [ll]
	long int serno;							// serial number for next resurrected molec.
	int ngausstbl;							// number of elements in gausstbl
	double *gausstbl;						// random numbers for diffusion
	int *expand;								// whether species expand with libmzr [i]
	} *molssptr;

/*********************************** Walls **********************************/

typedef struct wallstruct {
	int wdim;										// dimension number of perpendicular to wall
	int side;										// low side of space (0) or high side (1)
	double pos;									// position of wall along dim axis
	char type;									// properties of wall
	struct wallstruct *opp; 		// pointer to opposite wall
	} *wallptr;

/********************************* Reactions ********************************/

#define MAXORDER 3
#define MAXPRODUCT 16
enum RevParam {RPnone,RPirrev,RPconfspread,RPbounce,RPpgem,RPpgemmax,RPpgemmaxw,RPratio,RPunbindrad,RPpgem2,RPpgemmax2,RPratio2,RPoffset,RPfixed};

class ValueProvider;
typedef ValueProvider* valueproviderptr;
typedef struct rxnstruct {
	struct rxnsuperstruct *rxnss;	// pointer to superstructure
	char *rname;								// pointer to name of reaction
	int *rctident;							// list of reactant identities [rct]
	enum MolecState *rctstate;	// list of reactant states [rct]
	int *permit;								// permissions for reactant states [ms]
	int nprod;									// number of products
	int *prdident;							// list of product identities [prd]
	enum MolecState *prdstate;	// list of product states [prd]
	valueproviderptr rateValueProvider;			// requested reaction rate
	double rate;								// requested reaction rate
	double bindrad2;						// squared binding radius, if appropriate
	double prob;								// reaction probability
	double tau;									// characteristic reaction time
	enum RevParam rparamt;			// type of parameter in rpar
	double rparam;							// parameter for reaction of products
	double unbindrad;						// unbinding radius, if appropriate
	double **prdpos;						// product position vectors [prd][d]
	struct compartstruct *cmpt;	// compartment reaction occurs in, or NULL
	struct surfacestruct *srf;	// surface reaction on, or NULL
	} *rxnptr;

typedef struct rxnsuperstruct {
	enum StructCond condition;	// structure condition
	struct simstruct *sim;			// simulation structure
	int order;									// order of reactions listed: 0, 1, or 2
	int maxspecies;							// maximum number of species
	int maxlist;								// copy of maximum number of molecule lists
	int *nrxn;									// number of rxns for each reactant set [i]
	int **table;								// lookup table for reaction numbers [i][j]
	int maxrxn;									// allocated number of reactions
	int totrxn;									// total number of reactions listed
	char **rname;								// names of reactions [r]
	rxnptr *rxn;								// list of reactions [r]
	int *rxnmollist;						// live lists that have reactions [ll]
	} *rxnssptr;

/********************************* Surfaces *********************************/

#define PSMAX 6															// maximum number of panel shapes
enum PanelFace {PFfront,PFback,PFnone,PFboth};
enum PanelShape {PSrect,PStri,PSsph,PScyl,PShemi,PSdisk,PSall,PSnone};
enum SrfAction {SAreflect,SAtrans,SAabsorb,SAjump,SAport,SAmult,SAno,SAnone,SAadsorb,SArevdes,SAirrevdes,SAflip};
enum DrawMode {DMno=0,DMvert=1,DMedge=2,DMve=3,DMface=4,DMvf=5,DMef=6,DMvef=7,DMnone};
enum SMLflag {SMLno=0,SMLdiffuse=1,SMLreact=2,SMLsrfbound=4};

typedef struct surfactionstruct {
	int *srfnewspec;						// surface convert mol. species [ms]
	double *srfrate;						// surface action rate [ms]
	valueproviderptr* srfRateValueProvider;	//rate for surface actions: asorption, desorption, transmission...etc.
	double *srfprob;						// surface action probability [ms]
	double *srfcumprob;					// surface cumulative probability [ms]
	int *srfdatasrc;						// surface data source [ms]
	double *srfrevprob;					// probability of reverse action [ms]
	} *surfactionptr;

typedef struct panelstruct {
	char *pname;								// panel name (reference, not owned)
	enum PanelShape ps;					// panel shape
	struct surfacestruct *srf;	// surface that owns this panel
	int npts;										// number of defining points
	double **point;							// defining points, [number][d]
	double front[DIMMAX];				// front parameters, which depend on the shape
	struct panelstruct *jumpp[2];// panel to jump to, if appropriate [face]
	enum PanelFace jumpf[2];		// face to jump to, if appropriate [face]
	int maxneigh;								// maximum number of neighbor panels
	int nneigh;									// number of neighbor panels
	struct panelstruct **neigh;	// list of neighbor panels [p]
	double *emitterabsorb[2];		// absorption for emitters [face][i]
	} *panelptr;

typedef struct surfacestruct {
	char *sname;								// surface name (reference, not owned)
	struct surfacesuperstruct *srfss;	// owning surface superstructure
	enum SrfAction ***action;		// action for molecules [i][ms][face]
	surfactionptr ***actdetails;	// action details [i][ms][face]
	double fcolor[4];						// RGBA color vector for front
	double bcolor[4];						// RGBA color vector for back
	double edgepts;							// thickness of edge for drawing
	unsigned int edgestipple[2];	// edge stippling [factor,pattern]
	enum DrawMode fdrawmode;		// polygon drawing mode for front
	enum DrawMode bdrawmode;		// polygon drawing mode for back
	double fshiny;							// front shininess
	double bshiny;							// back shininess
	int maxpanel[PSMAX];				// allocated number of panels [ps]
	int npanel[PSMAX];					// actual number of panels [ps]
	char **pname[PSMAX];				// names of panels [ps][p]
	panelptr *panels[PSMAX];		// list of panels [ps][p]
	struct portstruct *port[2];	// port, if any, for each face [face]
	double totarea;							// total surface area
	int totpanel;								// total number of panels
	double *areatable;					// cumulative panel areas [pindex]
	panelptr *paneltable;				// sequential list of panels [pindex]
	int *maxemitter[2];					// maximum number of emitters [face][i]
	int *nemitter[2];						// number of emitters [face][i]
	double **emitteramount[2];	// emitter amounts [face][i][emit]
	double ***emitterpos[2];		// emitter positions [face][i][emit][d]
	 } *surfaceptr;

typedef struct surfacesuperstruct {
	enum StructCond condition;	// structure condition
	struct simstruct *sim;			// simulation structure
	int maxspecies;							// maximum number of molecular species
	int maxsrf;									// maximum number of surfaces
	int nsrf;										// number of surfaces
	double epsilon;							// max deviation of surface-point from surface
	double margin;							// panel margin away from edge
	double neighdist;						// neighbor distance value
	char **snames;							// surface names [s]
	surfaceptr *srflist;				// list of surfaces [s]
	int maxmollist;							// number of molecule lists allocated
	int nmollist;								// number of molecule lists used
	enum SMLflag *srfmollist;		// flags for molecule lists to check [ll]
	} *surfacessptr;

/*********************************** Boxes **********************************/

typedef struct boxstruct {
	int *indx;									// dim dimensional index of the box [d]
	int nneigh;									// number of neighbors in list
	int midneigh;								// logical middle of neighbor list
	struct boxstruct **neigh;		// all box neighbors, using sim. accuracy
	int *wpneigh;								// wrapping code of neighbors in list
	int nwall;									// number of walls in box
	wallptr *wlist;							// list of walls that cross the box
	int maxpanel;								// allocated number of panels in box
	int npanel;									// number of surface panels in box
	panelptr *panel;						// list of panels in box
	int *maxmol;								// allocated size of live lists [ll]
	int *nmol;									// number of molecules in live lists [ll]
	moleculeptr **mol;					// lists of live molecules in the box [ll][m]
	} *boxptr;

typedef struct boxsuperstruct {
	enum StructCond condition;	// structure condition
	struct simstruct *sim;			// simulation structure
	int nlist;									// copy of number of molecule lists
	double mpbox;								// requested number of molecules per box
	double boxsize;							// requested box width
	double boxvol;							// actual box volumes
	int nbox;										// total number of boxes
	int *side;									// number of boxes on each side of space
	double *min;								// position vector for low corner of space
	double *size;								// length of each side of a box
	boxptr *blist; 							// actual array of boxes
	} *boxssptr;

/******************************* Compartments *******************************/

enum CmptLogic {CLequal,CLequalnot,CLand,CLor,CLxor,CLandnot,CLornot,CLnone};

typedef struct compartstruct {
	struct compartsuperstruct *cmptss;	// compartment superstructure
	char *cname;								// compart. name (reference, not owned)
	int nsrf;										// number of bounding surfaces
	surfaceptr *surflist;				// list of bounding surfaces [s]
	int npts;										// number of inside-defining points
	double **points;						// list of inside-defining points [k][d]
	int ncmptl;									// number of logic compartments
	struct compartstruct **cmptl;	// list of logic compartments [cl]
	enum CmptLogic *clsym;			// compartment logic symbol [cl]
	double volume;							// volume of compartment
	int maxbox;									// maximum number of boxes in compartment
	int nbox;										// number of boxes inside compartment
	boxptr *boxlist;						// list of boxes inside compartment [b]
	double *boxfrac;						// fraction of box volume that's inside [b]
	double *cumboxvol;					// cumulative cmpt. volume of boxes [b]
	} *compartptr;

typedef struct compartsuperstruct {
	enum StructCond condition;	// structure condition
	struct simstruct *sim;			// simulation structure
	int maxcmpt;								// maximum number of compartments
	int ncmpt;									// actual number of compartments
	char **cnames;							// compartment names [c]
	compartptr *cmptlist;				// list of compartments [c]
	} *compartssptr;

/*********************************** Ports **********************************/

typedef struct portstruct {
	struct portsuperstruct *portss;	// port superstructure
	char *portname;							// port name (reference, not owned)
	surfaceptr srf;							// porting surface (ref.)
	enum PanelFace face;				// active face of porting surface
	int llport;									// live list number for buffer
	} *portptr;

typedef struct portsuperstruct {
	enum StructCond condition;	// structure condition
	struct simstruct *sim;			// simulation structure
	int maxport;								// maximum number of ports
	int nport;									// actual number of ports
	char **portnames;						// port names
	portptr *portlist;					// list of ports
	} *portssptr;

/********************************* Filaments ********************************/

typedef struct filamentstruct {
	struct filamentsuperstruct *filss;	// filament superstructure
	char *fname;								// filament name
	int nmax;				// number of monomers allocated [1,inf)
	int n;					// number of monomers
	double **px;		// Coords. for monomer ends [nmax+1][3]
	double *pl;			// monomer length [nmax]
	double **pa;		// relative ypr angles [nmax][3]
	double **pd;		// relative dcm [nmax][9]
	double **po;		// absolute monomer orientation [9]
	double *pthk;		// thickness of monomer [nmax], [0,inf)
	double lstd;		// minimum energy monomer length
	double astd[3];	// minimum energy bend angle
	double lk;			// force constant for length
	double ak[3];		// force constant for angle
	double kT;			// thermodynamic temperature, [0,inf)
	char surf;			// character for surface shape
	double spar[2];// parameters of surface
	} *filamentptr;

typedef struct filamentsuperstruct {
	enum StructCond condition;			// structure condition
	struct simstruct *sim;					// simulation structure
	int maxfil;											// maximum number of filaments
	int nfil;												// actual number of filaments
	char **fnames;								// filament names
	filamentptr *fillist;						// list of filaments
	} *filamentssptr;


/******************************* Moleculizer ********************************/

typedef struct mzrsuperstruct {
	enum StructCond condition;				// structure condition
	struct simstruct *sim;						// simulation structure
	struct moleculizer_handle *mzr;		// moleculizer object
	int ruleschars;										// number of characters in rules
	char *rules;											// input rules file
	int maxstreams;										// allocated number of species streams
	int nstreams;											// actual number of species streams
	char **streamname;								// names of species streams [strm]
	double **displaysize;							// display sizes for streams [strm][ms]
	double ***color;									// colors for streams [strm][ms][c]
	double **strmdifc;								// diff. coeff. for streams [strm][ms]
	int maxNetworkSpecies;						// maximum expansion size of network
	unsigned int maxnamehash;					// allocated size of name hash
	unsigned int nnamehash;						// actual size of name hash
	char **tagname;										// hash list of mzr tagged names
	char **smolname;									// hash list of Smoldyn names
	int maxrxnhash;										// allocated size of reaction hash
	int nrxnhash;											// actual size of reaction hash
	char **mzrrxn;										// hash list of mzr reaction names
	char **smolrxn;										// hash list of Smoldyn reaction names
	int maxspecies;										// allocated size of species list
	enum MolecState *defaultstate;		// default state for each species [i]
	int refspecies;										// species for diffusion coeff. reference
	double refmass;										// mass of reference species
	double refdifc[MSMAX];						// diffusion coefficients of ref. species
	int expandall;										// flag for full expansion at initialize
	} *mzrssptr;

/******************************** Threading ********************************/
//?? nothing in this section is documented yet
#define BASE_STACK_SIZE sizeof(int) * 16;

typedef struct stackstruct { // Used internally by threads for input/output
	void* stack_data;
	size_t current_size;
	size_t max_size;
	} stack;

typedef void* thread_t;

typedef struct threadstruct { // All the data needed to execute a thread.
	thread_t thread_id;
	stack* input_stack;
	stack* output_stack;
	} *threadptr;

typedef struct threadingsuperstruct { // Master structure that contains all the information for all threads.  
	threadptr* thread;
	int nthreads;
	} *threadssptr;

/********************************* Graphics ********************************/

#define MAXLIGHTS 8						// must be ? GL_MAX_LIGHTS
enum LightParam {LPambient,LPdiffuse,LPspecular,LPposition,LPon,LPoff,LPauto,LPnone};

typedef struct graphicssuperstruct {
	enum StructCond condition;	// structure condition
	struct simstruct *sim;			// simulation structure
	int graphics;								// graphics: 0=none, 1=opengl, 2=good opengl
	int runmode;								// 0=Smoldyn, 1=Libsmoldyn
	int currentit;							// current number of simulation time steps
	int graphicit;							// number of time steps per graphics update
	unsigned int graphicdelay;	// minimum delay (in ms) for graphics updates
	int tiffit;									// number of time steps per tiff save
	double framepts;						// thickness of frame for graphics
	double gridpts;							// thickness of virtual box grid for graphics
	double framecolor[4];				// frame color [c]
	double gridcolor[4];				// grid color [c]
	double backcolor[4];				// background color [c]
	double textcolor[4];				// text color [c]
	int maxtextitems;						// allocated size of item list
	int ntextitems;							// actual size of item list
	char **textitems;						// items to display with text [item]
	enum LightParam roomstate;	// on, off, or auto (on)
	double ambiroom[4];					// global ambient light [c]
	enum LightParam lightstate[MAXLIGHTS];	// on, off, or auto (off) [lt]
	double ambilight[MAXLIGHTS][4];		// ambient light color [lt][c]
	double difflight[MAXLIGHTS][4];		// diffuse light color [lt][c]
	double speclight[MAXLIGHTS][4];		// specular light color [lt][c]
	double lightpos[MAXLIGHTS][3];		// light positions [lt][d]
	} *graphicsssptr;

/******************************** Simulation *******************************/

#define ETMAX 10
enum SmolStruct {SSmolec,SSwall,SSrxn,SSsurf,SSbox,SScmpt,SSport,SSfilament,SScmd,SSmzr,SSsim,SScheck,SSall,SSnone};
enum EventType {ETwall,ETsurf,ETdesorb,ETrxn0,ETrxn1,ETrxn2intra,ETrxn2inter,ETrxn2wrap,ETimport,ETexport};

typedef int (*diffusefnptr)(struct simstruct *);
typedef int (*surfaceboundfnptr)(struct simstruct *,int);
typedef int (*surfacecollisionsfnptr)(struct simstruct *,int,int);
typedef int (*assignmols2boxesfnptr)(struct simstruct *,int,int);
typedef int (*zeroreactfnptr)(struct simstruct *);
typedef int (*unimolreactfnptr)(struct simstruct *);
typedef int (*bimolreactfnptr)(struct simstruct *,int);
typedef int (*checkwallsfnptr)(struct simstruct *,int,int,boxptr);

struct CompartmentIdentifierPair {
	char name[128];
	unsigned char pixel;//the compartmentID
};

typedef struct VolumeSamples {
	int num[3];//number of mesh points in X, Y,Z
	double size[3];//actual size in X, Y, Z (e.g. in micron)
	double origin[3];//origin of the X, Y, Z
	unsigned char* volsamples;//compartmentID for each mesh point center
	int nCmptIDPair; // number of compartments
	CompartmentIdentifierPair* compartmentIDPairPtr;//ID vs. comptName pairs.
}* VolumeSamplesPtr;

class ValueProviderFactory;
class AbstractMesh;

typedef struct simstruct {
	enum StructCond condition;	// structure condition
	FILE *logfile;							// file to send output
	char *filepath;							// configuration file path
	char *filename;							// configuration file name
	char *flags;								// command-line options from user
	time_t clockstt;						// clock starting time of simulation
	double elapsedtime;					// elapsed time of simulation
	long int randseed;					// random number generator seed
	int eventcount[ETMAX];			// counter for simulation events
	int dim;										// dimensionality of space.
	double accur;								// accuracy, on scale from 0 to 10
	double time;								// current time in simulation
	double tmin;								// simulation start time
	double tmax;								// simulation end time
	double tbreak;							// simulation break time
	double dt;									// simulation time step
	rxnssptr rxnss[MAXORDER];		// reaction superstructures
	molssptr mols;							// molecule superstructure
	wallptr *wlist;							// list of walls
	surfacessptr srfss;					// surface superstructure
	boxssptr boxs;							// box superstructure
	compartssptr cmptss;				// compartment superstructure
	portssptr portss;						// port superstructure
	filamentssptr filss;				// filament superstructure
	mzrssptr mzrss;							// network generation rule superstructure
	void* cmds;									// command superstructure
	graphicsssptr graphss;			// graphics superstructure
	threadssptr threads;				// pthreads superstructure
	diffusefnptr diffusefn;											// function for molecule diffusion
	surfaceboundfnptr surfaceboundfn;						// function for surface-bound molecules
	surfacecollisionsfnptr surfacecollisionsfn; // function for surface collisons
	assignmols2boxesfnptr assignmols2boxesfn;		// function that assigns molecs to boxes
	zeroreactfnptr zeroreactfn;									// function for zero order reactions
	unimolreactfnptr unimolreactfn;							// function for first order reactions
	bimolreactfnptr bimolreactfn;								// function for second order reactions
	checkwallsfnptr checkwallsfn;								// function for molecule collisions with walls

	VolumeSamplesPtr volumeSamplesPtr;
	ValueProviderFactory* valueProviderFactory;
	AbstractMesh* mesh;
	} *simptr;


class ValueProvider {
public:
	virtual ~ValueProvider(){};
	virtual double getConstantValue() = 0;
	virtual double getValue(double t, double x, double y, double z, rxnptr rxn)=0;
	virtual double getValue(double t, double x, double y, double z, rxnptr rxn, char* panelName)=0;
	virtual double getValue(double t, double x, double y, double z, surfactionptr actiondetails, char* panelName)=0;
};

class ValueProviderFactory {
public:
	virtual ~ValueProviderFactory(){};
	virtual ValueProvider* createValueProvider(string& rateExp)=0;
	void setSimptr(simptr sim){this->sim = sim;}
	simptr getSimptr(){return this->sim;}
private:
	simptr sim;
};

class AbstractMesh{
public:
	virtual ~AbstractMesh(){};
	virtual void getCenterCoordinates(int volIndex, double* coords)=0;
	virtual void getDeltaXYZ(double* delta)=0;
	virtual void getNumXYZ(int* num)=0;
};


#endif

