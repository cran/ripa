/** $Header: /database/repository/rimage/src/Attic/freqfilters.cpp,v 1.1.2.1 2003/03/15 07:44:18 tomo Exp $
 **
 ** Copyright (c) 2003 Nikon Digital Technologies Co., Ltd.
 ** For complete license terms see file LICENSE
 **/

#include "freqfilters.h"
#include <cmath>

Filter::Filter(Matrix_ripa& m) {

  data = new Matrix_ripa(m.getX(),m.getY());
  (*data)=m;
  res = new Matrix_ripa( (*data).getX(), (*data).getY() );

}


Filter::~Filter() {

  if( res!=NULL )
    delete(res);
  if( data!=NULL )
    delete(data);

}


Matrix_ripa& Filter::createNewRes() {

  Matrix_ripa *save = res;
  res = new Matrix_ripa( (*data).getX(), (*data).getY() );
  return *save;

}


void Filter::circle( int x, int y, int r ) {

  int xdim = (*data).getX();
  int ydim = (*data).getY();

  for( int i=0; i<xdim; i++ )
    for( int j=0; j<ydim; j++ )
      {
	double a = (i-x)*(i-x);
	double b = (j-y)*(j-y);
	double c = r*r;
	if( a+b<c )
	  (*res)[i][j] = 1.0;
	else
	  (*res)[i][j] = 0.0;
      }

}


void Filter::smooth3x3() {

  int xdim = (*data).getX();
  int ydim = (*data).getY();
  double norm = 1/12.0;

  for( int i=1; i<xdim-1; i++ )
    for( int j=1; j<ydim-1; j++ )
	(*res)[i][j] = norm * ( 
	  ( (*data)[i-1][j-1] + (*data)[i-1][j+1] + (*data)[i+1][j-1] + (*data)[i+1][j+1] ) + 
	  2 * ( (*data)[i-1][j] + (*data)[i+1][j] + (*data)[i][j-1] + (*data)[i][j+1] ) +
	  4 * (*data)[i][j] 
	  );

}


void Filter::smooth5x5() {

  int xdim = (*data).getX();
  int ydim = (*data).getY();
  double norm = 1/232.0;

  for( int i=2; i<xdim-2; i++ )
    for( int j=2; j<ydim-2; j++ )
	(*res)[i][j] = norm * ( 
			       ( (*data)[i-2][j-2] + (*data)[i-2][j+2] + 
				 (*data)[i+2][j-2] + (*data)[i+2][j+2] 
				 ) +
			       4 * 
			       ( 
				(*data)[i-2][j-1] + (*data)[i-1][j-2] + 
				(*data)[i+1][j-2] + (*data)[i+2][j-1] + 
				(*data)[i+2][j+1] + (*data)[i+1][j+2] + 
				(*data)[i-1][j+2] + (*data)[i-2][j+1]
				) 
			       + 16 * 
			       (
				(*data)[i-1][j-1] + (*data)[i+1][j+1] + 
				(*data)[i-1][j+1] + (*data)[i+1][j+1]
				)
			       + 24 * 
			       (
				(*data)[i-1][j] + (*data)[i+1][j] + 
				(*data)[i][j+1] + (*data)[i][j-1]
				) 
			       + 36 * (*data)[i][j]
			       );

}


void Filter::computeLowPass( int passDistance ) {

  int xcoord = (int) ceil((*data).getX()*0.5);
  int ycoord = (int) ceil((*data).getY()*0.5);
  circle( xcoord, ycoord, passDistance );
  swapData();
  smooth5x5();

}



void Filter::computeHighPass( int blockDistance ) {

  int xcoord = (int) ceil((*data).getX()*0.5);
  int ycoord = (int) ceil((*data).getY()*0.5);
  int xdim = (*data).getX();
  int ydim = (*data).getY();
  circle( xcoord, ycoord, blockDistance );
  for( int i=0; i<xdim; i++ )
    for( int j=0; j<ydim; j++ )
      (*res)[i][j] = 1 - (*res)[i][j];
  swapData();
  smooth5x5();

}



void Filter::computeBandPass( int lowBlock, int highBlock ) {

  int xcoord = (int) ceil((*data).getX()*0.5);
  int ycoord = (int) ceil((*data).getY()*0.5);
  circle( xcoord, ycoord, highBlock );
  Matrix_ripa& b1 = createNewRes();
  circle( xcoord, ycoord, lowBlock );
  Matrix_ripa& b2 = createNewRes();
  int xdim = (*data).getX();
  int ydim = (*data).getY();

  for( int i=0; i<xdim; i++ )
    for( int j=0; j<ydim; j++ )
      (*res)[i][j] = b1[i][j] - b2[i][j];

  delete(&b1);
  delete(&b2);
  swapData();
  smooth5x5();

}



void Filter::computeYSymmetricBandPass( int xdist, int ycoord, int size ) {

  int xmiddle = (int) ceil((*data).getX()*0.5);
  int ymiddle = (int) ceil((*data).getY()*0.5);

  circle( xmiddle-xdist, ymiddle, size );
  Matrix_ripa& b1 = createNewRes();
  circle( xmiddle+xdist, ymiddle, size );
  Matrix_ripa& b2 = createNewRes();

  int xdim = (*data).getX();
  int ydim = (*data).getY();

  for( int i=0; i<xdim; i++ )
    for( int j=0; j<ydim; j++ )
      {
	double sum = b1[i][j] + b2[i][j];
	if( sum>1.0 )
	  sum = 1.0;
	(*res)[i][j] = sum;
      }

  delete(&b1);
  delete(&b2);
  swapData();
  smooth5x5();

}



void Filter::computeXSymmetricBandPass( int xcoord, int ydist, int size ) {
  
  int xmiddle = (int) ceil((*data).getX()*0.5);
  int ymiddle = (int) ceil((*data).getY()*0.5);
  
  circle( xmiddle, ymiddle-ydist, size );
  Matrix_ripa& b1 = createNewRes();
  circle( xmiddle, ymiddle+ydist, size );
  Matrix_ripa& b2 = createNewRes();

  int xdim = (*data).getX();
  int ydim = (*data).getY();
  
  for( int i=0; i<xdim; i++ )
    for( int j=0; j<ydim; j++ )
      {
	double sum = b1[i][j] + b2[i][j];
	if( sum>1.0 )
	  sum = 1.0;
	(*res)[i][j] = sum;
      }
  
  delete(&b1);
  delete(&b2);
  swapData();
  smooth5x5();
  
}



void Filter::computeOrigSymmetricBandPass( int dist, int size ) {

  int xmiddle = (int) ceil((*data).getX()*0.5);
  int ymiddle = (int) ceil((*data).getY()*0.5);

  circle( xmiddle-dist, ymiddle-dist, size );
  Matrix_ripa& b1 = createNewRes();
  circle( xmiddle-dist, ymiddle+dist, size );
  Matrix_ripa& b2 = createNewRes();
  circle( xmiddle+dist, ymiddle-dist, size );
  Matrix_ripa& b3 = createNewRes();
  circle( xmiddle+dist, ymiddle+dist, size );
  Matrix_ripa& b4 = createNewRes();
  int xdim = (*data).getX();
  int ydim = (*data).getY();

  for( int i=0; i<xdim; i++ )
    for( int j=0; j<ydim; j++ )
      {
	double sum = b1[i][j] + b2[i][j] + b3[i][j] + b4[i][j];
	if( sum>1.0 )
	  sum = 1.0;
	(*res)[i][j] = sum;
      }

  delete(&b1);
  delete(&b2);
  delete(&b3);
  delete(&b4);
  swapData();
  smooth5x5();

}



void Filter::removeGlobalIllumination() {

  int xdim = (*data).getX();
  int ydim = (*data).getY();
  for( int i=0; i<xdim; i++ )
    for( int j=0; j<ydim; j++ )
      (*res)[i][j] = (*data)[i][j];

  double xmiddle = xdim * 0.5;
  double ymiddle = ydim * 0.5;
  int xmfloor = (int) floor( xmiddle );
  int ymfloor = (int) floor( ymiddle );
  int xmceil = (int) ceil( xmiddle );
  int ymceil = (int) ceil( ymiddle );

  (*res)[xmfloor][ymfloor] = 0.0;
  (*res)[xmfloor][ymceil] = 0.0;
  (*res)[xmceil][ymfloor] = 0.0;
  (*res)[xmceil][ymceil] = 0.0;

}
