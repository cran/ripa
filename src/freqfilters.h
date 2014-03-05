/** $Header: /database/repository/rimage/src/Attic/freqfilters.h,v 1.1.2.1 2003/03/15 07:44:18 tomo Exp $
 **
 ** Copyright (c) 2003 Nikon Digital Technologies Co., Ltd.
 ** For complete license terms see file LICENSE
 **/

#include "matrix_ripa.h"
//#include "fftw_access_func.h"
#include <stdlib.h>
#include <stdio.h>

class Filter {

 private:
  Matrix_ripa *data; // *data must contain data in the frequency domain
  Matrix_ripa *res;
  Matrix_ripa& createNewRes();
  void circle( int x, int y, int r );

 public:
  Filter(Matrix_ripa& m);
  ~Filter();
  Matrix_ripa& getRes() { return *res; };
  void smooth3x3();
  void smooth5x5();
  void computeLowPass( int passDistance );
  void computeHighPass( int blockDistance );
  void computeBandPass( int lowBlock, int highBlock );
  void computeXSymmetricBandPass( int xdist, int ycoord, int size );
  void computeYSymmetricBandPass( int xcoord, int ydist, int size );
  void computeOrigSymmetricBandPass( int dist, int size );
  void removeGlobalIllumination();
  void swapData() { 
    Matrix_ripa *tmp = data;
    data = res; 
    res = tmp; 
    (*res).init();
  };

};
