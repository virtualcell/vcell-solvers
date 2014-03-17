
#pragma warning (disable: 4996 4267)
#include <cmath>
#include <boost/multiprecision/cpp_int.hpp> 
#include <boost/multiprecision/cpp_dec_float.hpp> 
#include <boost/polygon/voronoi.hpp>

#include <limits>
#include <gtest/gtest.h>

#include <iostream>
using namespace boost::polygon;

namespace vcellVoronoiImpl {
	using boost::multiprecision::et_off;
    typedef boost::multiprecision::number<boost::multiprecision::cpp_dec_float<48>,et_off> floatingType;

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

    struct voronoi_ctype_traits {
        typedef boost::int64_t int_type;
        typedef boost::multiprecision::int128_t int_x2_type;
        typedef boost::multiprecision::uint128_t uint_x2_type;
        typedef boost::multiprecision::int512_t big_int_type;
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


