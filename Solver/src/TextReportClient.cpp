#include <MPoint.h>
#include <iomanip>
#include <MeshElementNode.h>
#include <MovingBoundaryParabolicProblem.h>
#include <World.h>
#include <vcellxml.h>
#include <vector>
#include <TextReportClient.h>
#include <vcellxml.h>
#pragma GCC diagnostic ignored "-Winvalid-offsetof"
using std::endl;
using spatial::cX;
using spatial::cY;

namespace moving_boundary
{
TextReportClient::TextReportClient(const std::string & a_filename, MovingBoundaryParabolicProblem &mbpp, int precision,
		int width, long interval) :
		filename(a_filename),
		output(filename),
		problem(mbpp),
		pointConverter(World<CoordinateType, 2>::get().pointConverter()),
		outputPrecision(precision),
		outputWidth(width),
		keeyEvery(interval),
		iterCount(-1),
		lastIteration(false)
{
	output.precision(outputPrecision);
	output.setf(std::ios_base::fixed, std::ios_base::floatfield); //fixed
}

TextReportClient::~TextReportClient()
{
	try
	{
		output.close();
	}
	catch (...)
	{
		// ignore
	}
}
std::string TextReportClient::outputName( ) const
{
	return filename;
}

std::string TextReportClient::getXML( ) const
{
	return "";
}

bool TextReportClient::shouldReport()
{
	return iterCount == 0 || lastIteration || iterCount % keeyEvery == 0;
}

void TextReportClient::time(double t, unsigned int numIteration, bool last,
		const GeometryInfo<CoordinateType> &geometryInfo)
{
	iterCount = numIteration;
	lastIteration = last;
	if (!shouldReport())
	{
		return;
	}
	if (t == 0)
	{
		output << "%%%%%% MovingBoundarySolver output" << endl;
		output << "Nx=" << problem.setup().maxTime << endl;
		output << "Tmax=" << problem.setup().maxTime << endl;
		output << "precision=  " << outputPrecision << ";" << endl;
		output << "width=  " << outputWidth << ";" << endl;
	}
	output << "iter= " << numIteration << ";" << endl;
	output << "time= " << std::setw(outputWidth) << t << ";" << endl;
	if (lastIteration)
	{
		output << "%%% last iteration" << endl;
	}
	output << "nodesAdjusted_" << numIteration << "= " << geometryInfo.nodesAdjusted << ";" << endl;
	std::vector<spatial::TPoint<CoordinateType,2> > front = geometryInfo.boundary;
	output << "front_" << numIteration << "_size=" << front.size() << ";" << endl;
	output << "%%% front=[x y Vx Vy];" << endl;
	output << "front_" << numIteration << "=[ " << endl;
	for (auto iter = front.begin(); iter != front.end(); ++iter)
	{
		auto simPoint = *iter;
		auto pdPoint = pointConverter(simPoint);
//		spatial::SVector<moving_boundary::VelocityType,2> v = problem.velocity(simPoint);
		//output << simPoint << "; (" << pdPoint << ')' << endl;
		output << pdPoint(cX) << " " << pdPoint(cY)
				//<< " " << v(cX) << " " << v(cY)
						<< endl;
	}
	output << "];" << endl << endl;
	output << "%%% elements=[x y u v m];" << endl;
	output << "elements_" << numIteration << "=[" << endl;
}

void TextReportClient::element(const MeshElementNode &e)
{
	if (!shouldReport())
	{
		return;
	}
	auto pdPoint = pointConverter(e);
	output << pdPoint(cX) << " " << pdPoint(cY) << " " << std::setw(outputWidth)
			<< e.concentration(0) << " " << std::setw(outputWidth) << e.volumePD()
			<< " " << std::setw(outputWidth) << e.mass(0) << endl;
}

void TextReportClient::iterationComplete()
{
	if (!shouldReport())
	{
		return;
	}
	output << "];" << endl;
	output << "%%% end of generation " << iterCount << endl;
}

void TextReportClient::simulationComplete()
{
	output << "%%% end of simulation " << iterCount << endl;
}
}

