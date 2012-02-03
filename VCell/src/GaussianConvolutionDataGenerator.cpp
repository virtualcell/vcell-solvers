/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/GaussianConvolutionDataGenerator.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/CartesianMesh.h>
#include <VCELL/Element.h>
#include <Expression.h>
using VCell::Expression;

#include <algorithm>
using std::max;
using std::min;

GaussianConvolutionDataGenerator::GaussianConvolutionDataGenerator(string& name, Feature* feature, double sigmaXY, double sigmaZ, Expression* func) 
	: DataGenerator(name, feature){
		this->sigmaXY = sigmaXY;
		this->sigmaZ = sigmaZ;
		sigmaRatio = sigmaZ/sigmaXY;
	this->function = func;
	gaussianPsfSamples = NULL;
	functionValues = NULL;
}

GaussianConvolutionDataGenerator::~GaussianConvolutionDataGenerator() {
	delete function;
	delete[] gaussianPsfSamples;
	delete[] functionValues;
}

void GaussianConvolutionDataGenerator::resolveReferences(SimulationExpression* sim) {
	function->bindExpression(sim->getSymbolTable());

	// compute data size
	CartesianMesh* mesh = (CartesianMesh*)sim->getMesh();
	dataSize = mesh->getNumVolumeX() * mesh->getNumVolumeY() * mesh->getNumVolumeZ();
	data = new double[dataSize];
	memset(data, 0, dataSize * sizeof(double));

	functionValues = new double[dataSize];
	memset(functionValues, 0, dataSize * sizeof(double));

	double dx = mesh->getXScale_um();
	double dy = mesh->getYScale_um();
	double dz = mesh->getZScale_um();

	int numSigmas = 8;
	gaussianPsfSampleNx = sigmaXY * numSigmas / dx;
	gaussianPsfSampleNy = mesh->getDimension() > 1 ? sigmaXY * numSigmas / dy : 1;
	gaussianPsfSampleNz = mesh->getDimension() > 2 ? sigmaZ * numSigmas / dz : 1;
	if (gaussianPsfSampleNx % 2 == 0) {
		++gaussianPsfSampleNx;
	}
	if (gaussianPsfSampleNy % 2 == 0) {
		++gaussianPsfSampleNy;
	}
	if (gaussianPsfSampleNz % 2 == 0) {
		++gaussianPsfSampleNz;
	}
	int gaussianPsfSampleNxy = gaussianPsfSampleNx * gaussianPsfSampleNy;
	gaussianPsfSamples = new double[gaussianPsfSampleNx * gaussianPsfSampleNy * gaussianPsfSampleNz];
	int midI = gaussianPsfSampleNx / 2;
	int midJ = gaussianPsfSampleNy / 2;
	int midK = gaussianPsfSampleNz / 2;
	double sum = 0;
	int volIndex = -1;
	for (int k = 0; k < gaussianPsfSampleNz; ++k) {
		double z = (k - midK) * dz;
		for (int j = 0; j < gaussianPsfSampleNy; ++j) {
			double y = (j - midJ) * dy;
			for (int i = 0; i < gaussianPsfSampleNx; ++i) {				
				double x = (i - midI) * dx;

				++ volIndex;
				double r = x*x + y*y + sigmaRatio*sigmaRatio*z*z;
				double d = exp(-(r*r)/(2*sigmaXY*sigmaXY));
				gaussianPsfSamples[volIndex] = d;
				sum += d;
			}
		}
	}
	for (int i = 0; i < gaussianPsfSampleNxy * gaussianPsfSampleNz; ++ i) {
		gaussianPsfSamples[i] /= sum;
	}
}

void GaussianConvolutionDataGenerator::computePPData(SimulationExpression* sim) {
	CartesianMesh* mesh = (CartesianMesh*)sim->getMesh();
	VolumeElement* volumeElements = mesh->getVolumeElements();
	int* indices = sim->getIndices();
	indices[VAR_MEMBRANE_INDEX] = -1;
	indices[VAR_MEMBRANE_REGION_INDEX] = -1;

	int numX = mesh->getNumVolumeX();
	int numY = mesh->getNumVolumeY();
	int numXY = numX * numY;
	int numZ = mesh->getNumVolumeZ();

	// precompute function values
	int volIndex = -1;
	for (int k = 0; k < numZ; ++k) {
		for (int j = 0; j < numY; ++j) {
			for (int i = 0; i < numX; ++i) {				
				++ volIndex;

				indices[VAR_VOLUME_INDEX] = volIndex;
				indices[VAR_VOLUME_REGION_INDEX] = volumeElements[volIndex].getRegionIndex();
				double value = function->evaluateProxy();								
				functionValues[volIndex] += value;

			} // end i
		} // end j
	} // end k

	int midI = gaussianPsfSampleNx / 2;
	int midJ = gaussianPsfSampleNy / 2;
	int midK = gaussianPsfSampleNz / 2;
	int gaussianPsfSampleNxy = gaussianPsfSampleNx * gaussianPsfSampleNy;
	volIndex = -1;
	for (int k = 0; k < numZ; ++k) {
		for (int j = 0; j < numY; ++j) {
			for (int i = 0; i < numX; ++i) {
				++ volIndex;

				data[volIndex] = 0;
				int kkOffset = 0;
				for (int kk = 0; kk < gaussianPsfSampleNz; kk ++, kkOffset += gaussianPsfSampleNxy) {
					int k2 = k - midK + kk;
					if (k2 < 0 || k2 >= numZ) {
						continue;
					}
					int jjOffset = kkOffset;
					for (int jj = 0; jj < gaussianPsfSampleNy; jj ++, jjOffset += gaussianPsfSampleNx) {
						int j2 = j - midJ + jj;
						if (j2 < 0 || j2 >= numY) {
							continue;
						}
						int iiOffset = jjOffset;
						for (int ii = 0; ii < gaussianPsfSampleNx; ii ++, ++iiOffset) {
							int i2 = i - midI + ii;
							if (i2 < 0 || i2 >= numX) {
								continue;
							}
							int psfIndex = iiOffset; 
							int volIndex2 = k2 * numXY + j2 * numX + i2;
							data[volIndex] += functionValues[volIndex2] * gaussianPsfSamples[psfIndex];
							
						} // end ii
					} // end jj
				} // end kk

			} // end i
		} // end j
	} // end k
}
