#include <iostream>
#include "gtest/gtest.h"
#include <Physiology.h>
#include <Species.h>
using namespace moving_boundary::biology;
namespace {
	void eval(const Species &sp, const std::vector<double> & v) {
		double ns = sp.evaluate(v);
		std::cout << ns << " " << sp.name( ) << std::endl;
	}
}
TEST(bio,physio) {
	Physiology physio;
	ASSERT_THROW(physio.numberSymbols( ),std::domain_error);
	physio.createSpecies("dog","1 + t * cat");
	physio.createSpecies("cat","2 * cat");
	std::array<std::string,1> syms = {"t"};

	physio.buildSymbolTable(syms);
	physio.lock( );

	std::vector<double> values(physio.numberSymbols( ));
	size_t idx = physio.symbolIndex("t");
	size_t cidx = physio.symbolIndex("cat");
	values[idx] = 2;
	values[cidx] = 10; 
	auto species = physio.species( );
	for (int i = 0; i < physio.numSpecies( ) ; ++i) {
		eval(species[i], values );
	}
	ASSERT_THROW(physio.createSpecies("alligator","3"), std::domain_error);



}
