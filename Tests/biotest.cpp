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
	physio.createSpecies("dog","1 + t * cat");
	physio.createSpecies("cat","2 * cat");
	std::array<std::string,1> syms = {"t"};

	physio.buildSymbolTable(syms);

	std::vector<double> values(physio.numberSymbols( ));
	size_t idx = physio.symbolIndex("t");
	size_t cidx = physio.symbolIndex("cat");
	values[idx] = 2;
	values[cidx] = 10; 
	for (int i = 0; i < physio.numSpecies( ) ; ++i) {
		eval(physio.species(i), values );
	}



}
