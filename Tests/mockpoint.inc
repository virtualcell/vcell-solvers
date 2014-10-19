/**
* common test fixture definitions
*/
namespace {
	struct IntMockPoint : spatial::MPoint<int,2> {
		IntMockPoint(const size_t *n, const int *values)
			:spatial::MPoint<int,2>(n,values) {} 
		const static int numSpecies=5;
		static spatial::DiffuseAdvectCache * createCache(const spatial::MeshDef<int,2> &) { return 0; }
	};
	struct MockPoint : spatial::MPoint<double,2> {
		MockPoint(spatial::MeshDef<double,2> & ,const size_t *n, const double *values)
			:spatial::MPoint<double,2>(n,values) {} 
		const static int numSpecies=5;
		static spatial::DiffuseAdvectCache * createCache(const spatial::MeshDef<double,2> &) { return 0; }
	};
	struct MockPoint3 : spatial::MPoint<double,3> {
		MockPoint3(spatial::MeshDef<double,3> &, const size_t *n, const double *values)
			:spatial::MPoint<double,3>(n,values) {} 
		const static int numSpecies=5;
		static spatial::DiffuseAdvectCache * createCache(const spatial::MeshDef<double,3> &) { return 0; }
	};
}
