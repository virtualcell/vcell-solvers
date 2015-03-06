#ifndef ExplicitSolver_h
#define ExplicitSolver_h
#include <MovingBoundaryTypes.h>
namespace spatial {
	template<class CT, int N, class TELEMENT> struct Mesh; 
}
namespace moving_boundary {
	struct MeshElementNode;
	struct ExplicitSolver {
		typedef spatial::Mesh<moving_boundary::CoordinateType,2,MeshElementNode> Mesh; 
		ExplicitSolver(Mesh &m);
		void setStepAndSpecies(TimeType t, unsigned int speciesIndex); 
		void setCoefficent(const MeshElementNode &j, BioQuanType);
		void setSolvingFor(MeshElementNode &i, BioQuanType);
		void solve( );
	private:
		void setMass(MeshElementNode &);
		Mesh &mesh;
		unsigned int sIdx;
		struct SetMass {
			SetMass(ExplicitSolver &s)
				:solver(s) {}
			void operator( )(MeshElementNode & e) {
				solver.setMass(e);
			}
			ExplicitSolver &solver;
		};
	};
}
#endif
