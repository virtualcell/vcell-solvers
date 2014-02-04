#ifndef MPoint_h
#define MPoint_h
#pragma warning ( disable : 4996)
#include <cassert>
#include <array>
#include <stdexcept>
#include <TPoint.h>
#include <infix_iterator.h>
namespace spatial {
	template<int N>
	class IndexInfo;

	/**
	* indexed point 
	* @tparam REAL coordinated type
	* @tparam N number of dimensions
	*/
	template<class REAL, int N>
	struct MPoint : public TPoint<REAL,N> {
		MPoint(const size_t *n, const REAL *values) {
			for (int i = 0; i < N; i++) {
				MPoint & us = *this;
				//us(static_cast<Axis>(i)) = values[i];
				TPoint<REAL,N>::coord[i] = values[i];
				index[i] = n[i];
			}
		}
		size_t indexOf(int dim) const {
			return index[dim];
		}

		const size_t * const indexes( ) const {
			return index.data( );
		}

		bool sameIndexes(const MPoint &rhs) const {
			return index == rhs.index;
		}

		IndexInfo<N> indexInfo( ) const {
			return IndexInfo<N>(index);
		}

		TPoint<size_t,N> indexPoint( ) const {
			return TPoint<size_t,N>(index);
		}


	protected:
		std::array<size_t,N> index;
	};

	enum SurfacePosition { unsetPosition = -1, deepInteriorSurface, interiorSurface, boundarySurface,outsideSurface, deepOutsideSurface };

	template<class REAL, int N>
	struct MeshElement : public MPoint<REAL,N> {
		typedef MPoint<REAL,N> base;
		MeshElement(const size_t *n, const REAL *values) 
			:base(n,values),
			mp(unsetPosition)
		{ }

		SurfacePosition mPos( ) const {
			return mp;
		}
		/**
		* point is inside boundary (#interiorSurface, #deepInteriorSurface or #boundarySurface)
		*/
		bool isInside( ) const {
			switch (mPos( )) {
			case interiorSurface:
			case deepInteriorSurface: 
			case boundarySurface:
				return true;
			case outsideSurface:
			case deepOutsideSurface:
				return false;
			default:
				throw std::logic_error("isInside unset position");
			}
		}
		/**
		* point is outside boundary (#outsideSurface,#deepOutsideSurface) 
		*/
		bool isOutside( ) const {
			return !isInside( );
		}

		/**
		* point is sufficiently far from boundary its position will not change in one time generation
		*/
		bool isDeep( ) const {
			switch (mPos( )) {
			case deepInteriorSurface: 
			case deepOutsideSurface:
				return true;
			case interiorSurface:
			case boundarySurface:
			case outsideSurface:
				return false;
			default:
				throw std::logic_error("isDeep unset position");
			}
		}
	protected:
		void setPos(SurfacePosition m)  {
			size_t x = this->index[0];
			size_t y = this->index[1];
			mp = m;
		}
		SurfacePosition mp;
	};

	inline std::ostream & operator<<(std::ostream & os, spatial::SurfacePosition mp) {
		switch (mp) {
		case spatial::deepInteriorSurface:
			os << "deep interior";
			break;
		case spatial::interiorSurface:
			os << "interior";
			break;
		case spatial::outsideSurface:
			os << "outside";
			break;
		case spatial::deepOutsideSurface:
			os << "deep outside";
			break;
		case spatial::boundarySurface:
			os << "boundary";
		break;
		default:
			assert(0);
		}
		return os;
	}
	/**
	* proxy class for pretty printing index information
	*/
	template<int N>
	class IndexInfo {
		const std::array<size_t,N> & index;
	public:
		IndexInfo(const std::array<size_t,N> & i)
			:index(i) {}
		void write(std::ostream &os) {
			os << '[';
			std::copy(index.begin( ), index.end( ),
				infix_ostream_iterator<size_t>(os,",") );
			os << ']';
		}
	};

	template<int N>
	inline std::ostream & operator<<(std::ostream & os, spatial::IndexInfo<N> ii) {
		ii.write(os);
		return os;
	}

}
#endif
