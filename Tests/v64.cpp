
#pragma warning (disable: 4996 4267)
#include <cmath>
#include <boost/multiprecision/cpp_int.hpp> 
#include <boost/multiprecision/cpp_dec_float.hpp> 
#include <boost/polygon/voronoi.hpp>

#include <limits>
#include <gtest/gtest.h>

#include <iostream>
using namespace boost::polygon;
namespace std {
	boost::multiprecision::number<boost::multiprecision::backends::cpp_dec_float<80> >
	sqrt(const boost::multiprecision::number<boost::multiprecision::backends::cpp_dec_float<80> >& in) {
		boost::multiprecision::backends::cpp_dec_float<80>  v = in.canonical_value(in);
		return v.calculate_sqrt( );
	}
}

namespace vcellVoronoiImpl {
	namespace bmulti = boost::multiprecision;
	using bmulti::et_off;
    typedef bmulti::number<bmulti::cpp_dec_float<80>,et_off> floatingType;

    struct FloatingConverter {
        template <typename T>
        floatingType operator( )(T x)  const {
            return static_cast<floatingType>(x);
        }
    };

    struct compareFloatingType {
        enum Result {
            LESS = -1,
            EQUAL = 0,
            MORE = 1
        };

        Result operator()(floatingType a, floatingType b, unsigned int maxUlps) const {
            if (a > b) {
                return a - b <= maxUlps ? EQUAL : LESS;
            }
            return b - a  <= maxUlps ? EQUAL : MORE;
        }
    };
	using bmulti::cpp_int_backend;
	using bmulti::signed_magnitude;
	using bmulti::unsigned_magnitude;
	using bmulti::unchecked;
	typedef bmulti::number<cpp_int_backend<128, 128, signed_magnitude, unchecked, void>,et_off >    int128;
	typedef bmulti::number<cpp_int_backend<128, 128, unsigned_magnitude, unchecked, void>,et_off >    uint128;
	typedef bmulti::number<cpp_int_backend<512, 512, signed_magnitude, unchecked, void>,et_off >    int512;


    struct voronoi_ctype_traits {
        typedef boost::int64_t int_type;
        typedef int128 int_x2_type;
        typedef uint128 uint_x2_type;
        typedef int512 big_int_type;
        typedef floatingType fpt_type;
        typedef floatingType efpt_type;
        typedef compareFloatingType ulp_cmp_type;
        typedef FloatingConverter to_fpt_converter_type;
        typedef FloatingConverter to_efpt_converter_type;
    };

    struct vcell_vd_traits{
        typedef voronoi_ctype_traits::fpt_type coordinate_type;
        typedef voronoi_cell<coordinate_type> cell_type;
        typedef voronoi_vertex<coordinate_type> vertex_type;
        typedef voronoi_edge<coordinate_type> edge_type;
        typedef struct {
        public:
            enum { ULPS = 128 };
            bool operator()(const vertex_type &v1, const vertex_type &v2) const {
                return (ulp_cmp(v1.x(), v2.x(), ULPS) == compareFloatingType::EQUAL &&
                    ulp_cmp(v1.y(), v2.y(), ULPS) == compareFloatingType::EQUAL);
            }
        private:
            compareFloatingType ulp_cmp;
        } vertex_equality_predicate_type;


    };
}

using namespace vcellVoronoiImpl;

TEST(highv,build) {
    voronoi_builder<boost::int64_t, vcellVoronoiImpl::voronoi_ctype_traits> vb;
    voronoi_diagram<boost::int64_t,vcell_vd_traits> vd;
   vb.construct(&vd);
}
TEST(highv,size) {
	std::cout << std::numeric_limits<long double>::digits << std::endl;
}


