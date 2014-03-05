/**

   Copyright (c) 2003 Nikon Digital Technologies Co., Ltd.
   complete license terms see file LICENSE

   attention:

   currently, this is a C-style library that uses some of the features 
   C++ offers.
   With an increase of functionality (and an increase of familarity between 
   the functions)
   that has to be changed.

**/

#include <math.h>
#include <assert.h>
#include "matrix_ripa.h"


Matrix_ripa& computeEqualization( Matrix_ripa& m );
inline int summarize( vector< int >& h, int n );
inline vector< int >& computeHistogram( Matrix_ripa &m );




extern "C" {

  void equalize( double *img, int *x, int *y, double *res ) {

    assert(img);
    assert(x);
    assert(y);
    assert(res);

    Matrix_ripa &m = mapRtoC( img, *x, *y );


    // do equalization
    Matrix_ripa& eq = computeEqualization( m );

    mapCtoR( eq, res );

    // cleanup...
    delete( &m );
    delete( &eq );

  }

}



/**
   we pressume 256 gray levels!!!
**/
Matrix_ripa& computeEqualization( Matrix_ripa& m ) {

  int xdim = m.getX();
  int ydim = m.getY();

  vector< int >& histo = computeHistogram( m );
  Matrix_ripa& eq = *(new Matrix_ripa(xdim,ydim) );

  vector< int > map(256);

  double fac = 1.0 / ( m.getX() * m.getY() );
  for( unsigned int i=0 ; i<histo.size() ; i++ ) 
    {
      double s = summarize(histo,i);
      double r = s * 255 * fac;
      map[i] = (int) rint( r );
    }

  for( int i=0 ; i<xdim ; i++ ) 
    for( int j=0 ; j<ydim ; j++ )
      {
	double rnd = rint( m[i][j] );
	eq[i][j] = map[ (int) rnd ];
      }

  delete( &histo );
  return eq;

}


inline int summarize( vector< int >& h, int n ) {

  int res=0;

  for( int i=0 ; i<n ; i++ )
    res += h[i];

  return res;

}



inline vector< int >& computeHistogram( Matrix_ripa &m ) {

  vector< int >& histo = *(new vector< int >(256));
  for( int i=0 ; i<m.getX() ; i++ )
    for( int j=0 ; j<m.getY() ; j++ )
      {
	double rnd = rint( m[i][j] );
	histo[ (int) rnd ] += 1;
      }

  return histo;

}
