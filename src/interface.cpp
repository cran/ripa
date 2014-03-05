#include <assert.h>
#include "freqfilters.h"

using namespace std;


extern "C" {

  void getLowPass( double *img, int *x, int *y, int *radius, double *res ) {

    assert(img);
    assert(x);
    assert(y);
    assert(radius);
    assert(res);

    Matrix_ripa m(*x,*y);
    for( int i=0; i<*x; i++ )
      for( int j=0; j<*y; j++ )
	m[i][j] = img[ i*(*y) + j ];

    Filter f(m);
    f.computeLowPass(*radius);
    Matrix_ripa &lp = f.getRes();

    for( int i=0; i<(*x); i++ )
      for( int j=0; j<(*y); j++ )
	res[ i*(*y) + j ] = lp[i][j];

  }


  void getHighPass( double *img, int *x, int *y, int *radius, double *res ) {

    assert(img);
    assert(x);
    assert(y);
    assert(radius);
    assert(res);

    Matrix_ripa m(*x,*y);
    for( int i=0; i<*x; i++ )
      for( int j=0; j<*y; j++ )
	m[i][j] = img[ i*(*y) + j ];

    Filter f(m);
    f.computeHighPass(*radius);
    Matrix_ripa &hp = f.getRes();

    for( int i=0; i<(*x); i++ )
      for( int j=0; j<(*y); j++ )
	res[ i*(*y) + j ] = hp[i][j];

  }


  void getBandPass( double *img, int *x, int *y, int *l_radius, int *h_radius, double *res ) {

    assert(img);
    assert(x);
    assert(y);
    assert(l_radius);
    assert(h_radius);
    assert(res);

    Matrix_ripa m(*x,*y);
    for( int i=0; i<*x; i++ )
      for( int j=0; j<*y; j++ )
	m[i][j] = img[ i*(*y) + j ];

    Filter f(m);
    f.computeBandPass(*l_radius,*h_radius);
    Matrix_ripa &lp = f.getRes();

    for( int i=0; i<(*x); i++ )
      for( int j=0; j<(*y); j++ )
	res[ i*(*y) + j ] = lp[i][j];

  }


  void smooth5( double *img, int *x, int *y, double *res ) {

    assert(img);
    assert(x);
    assert(y);
    assert(res);

    Matrix_ripa m(*x,*y);
    for( int i=0; i<*x; i++ )
      for( int j=0; j<*y; j++ )
	m[i][j] = img[ i*(*y) + j ];

    Filter f(m);
    f.smooth5x5();
    Matrix_ripa &smooth = f.getRes();

    for( int i=0; i<(*x); i++ )
      for( int j=0; j<(*y); j++ )
	res[ i*(*y) + j ] = smooth[i][j];

  }


  void getXSymmetricBandPass( double *img, int *x, int *y, int *xdist, int *ycoord, int *radius, double *res ) {

    assert(img);
    assert(x);
    assert(y);
    assert(xdist);
    assert(ycoord);
    assert(radius);
    assert(res);

    Matrix_ripa m(*x,*y);
    for( int i=0; i<*x; i++ )
      for( int j=0; j<*y; j++ )
	m[i][j] = img[ i*(*y) + j ];

    Filter f(m);
    
    f.computeXSymmetricBandPass(*xdist, *ycoord, *radius);
    Matrix_ripa &lp = f.getRes();

    for( int i=0; i<(*x); i++ )
      for( int j=0; j<(*y); j++ )
	res[ i*(*y) + j ] = lp[i][j];

  }


  void getYSymmetricBandPass( double *img, int *x, int *y, int *xcoord, int *ydist, int *radius, double *res ) {

    assert(img);
    assert(x);
    assert(y);
    assert(xcoord);
    assert(ydist);
    assert(radius);
    assert(res);

    Matrix_ripa m(*x,*y);
    for( int i=0; i<*x; i++ )
      for( int j=0; j<*y; j++ )
	m[i][j] = img[ i*(*y) + j ];

    Filter f(m);
    f.computeYSymmetricBandPass(*xcoord, *ydist, *radius);
    Matrix_ripa &lp = f.getRes();

    for( int i=0; i<(*x); i++ )
      for( int j=0; j<(*y); j++ )
	res[ i*(*y) + j ] = lp[i][j];

  }


  void getOrigSymmetricBandPass( double *img, int *x, int *y, int *dist, int *radius, double *res ) {

    assert(img);
    assert(x);
    assert(y);
    assert(dist);
    assert(radius);
    assert(res);

    Matrix_ripa m(*x,*y);
    for( int i=0; i<*x; i++ )
      for( int j=0; j<*y; j++ )
	m[i][j] = img[ i*(*y) + j ];

    Filter f(m);
    f.computeOrigSymmetricBandPass(*dist, *radius);
    Matrix_ripa &lp = f.getRes();

    for( int i=0; i<(*x); i++ )
      for( int j=0; j<(*y); j++ )
	res[ i*(*y) + j ] = lp[i][j];

  }


  void removeGlobalIllumination( double *img, int *x, int *y, double *res ) {

    assert(img);
    assert(x);
    assert(y);

    Matrix_ripa m(*x,*y);
    for( int i=0; i<*x; i++ )
      for( int j=0; j<*y; j++ )
	m[i][j] = img[ i*(*y) + j ];

    Filter f(m);
    f.removeGlobalIllumination();
    Matrix_ripa &re = f.getRes();

    for( int i=0; i<(*x); i++ )
      for( int j=0; j<(*y); j++ )
	res[ i*(*y) + j ] = re[i][j];

  }


}
