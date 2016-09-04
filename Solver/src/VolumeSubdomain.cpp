#include <VolumeSubdomain.h>
#include <cstring>
using moving_boundary::Subdomain;
using moving_boundary::VolumeSubdomain;

VolumeSubdomain::VolumeSubdomain(const string & name)
				:Subdomain(name)
{
}

VolumeSubdomain::~VolumeSubdomain()
{

}
