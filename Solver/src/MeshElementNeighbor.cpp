#include <MeshElementSpecies.h>
using namespace moving_boundary;
MeshElementNeighbor::MeshElementNeighbor(std::istream & is,  const MeshElementSpecies & client)  {
	vcell_persist::Token::check<MeshElementNeighbor>(is);
	spatial::ElementOffset<2> eo(is);
	element = client.neighbor(eo);
	vcell_persist::binaryRead(is,distanceTo);
	vcell_persist::binaryRead(is,edgeLength);
}

void MeshElementNeighbor::persist(std::ostream &os, const MeshElementSpecies &client) {
	vcell_persist::Token::insert<MeshElementNeighbor>(os);
	spatial::ElementOffset<2> eo = client.offset(*element);
	eo.persist(os);
	vcell_persist::binaryWrite(os,distanceTo);
	vcell_persist::binaryWrite(os,edgeLength);
}