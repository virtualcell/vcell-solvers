#ifndef _INDEXVECT_H_
#define _INDEXVECT_H_

#include <array>

#define DIM 2
class IndexVect
{
public:
  static const IndexVect Zero;
  static const IndexVect Unit;

  IndexVect ();
  IndexVect (int a, int b);
  IndexVect (const IndexVect& rhs);
  IndexVect(std::array<size_t, 2> p);

  IndexVect& operator= (const IndexVect& rhs);

  const int& operator[] (int i) const;
  int& operator[] (int i);

  IndexVect operator+ (const IndexVect& p) const;
  IndexVect& operator+= (const IndexVect& p);

  IndexVect operator- (const IndexVect& p) const;
  IndexVect& operator-= (const IndexVect& p);

  IndexVect operator* (int s) const;
  IndexVect& operator*= (int s);

  IndexVect operator/ (int s) const;
  IndexVect& operator/= (int s);

  int size() { return 2; }

  friend std::ostream& operator<< (std::ostream& ostr,
                                   const IndexVect& p);

private:
  int vect[DIM];

};

#endif
