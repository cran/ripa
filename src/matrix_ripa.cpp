/** 
 **  Copyright (c) 2003 Nikon Digital Technologies Co., Ltd.
 **  For complete license terms see file LICENSE
 **/

#include <assert.h>
#include "matrix_ripa.h"
#include <Rcpp.h>
using namespace Rcpp;

Matrix_ripa::Matrix_ripa() {

  m = NULL;
  x=-1; y=-1; 
  //cerr << "*** constructed Matrix_ripa object " << (int)this << " ***" << endl;

}


Matrix_ripa::Matrix_ripa(int i, int j) {

  //cout << " Matrix_ripa(int,int): " << i << "/" << j << endl;
//   m.resize(i);
//   //cout << "hier\n";
//   for(int n=0;n<i;n++)
//     {
//       m[n].resize(j);
//       //cerr << "n=" <<n << endl;
//       //getchar();
//     }
  x = i;
  y = j;
  m = new vector< vector<double> >();
  (*m).resize(i);
      for ( int ii=0; ii<i; ii++ )
 	(*m)[ii].resize(j);

  // (i,j)
  //cerr << "*** constructed Matrix_ripa object " << (int)this << " ***" << endl;
  //cerr << "leaving..." << endl;
  //init();
}


Matrix_ripa::Matrix_ripa( Matrix_ripa &ma ) {

  //cerr << "*** entered Matrix_ripa::Matrix_ripa( Matrix_ripa& ma ) ***" << endl;
  //cerr << "address of ma = " << (int)&ma << endl;
  //cerr << "ma.getX() = " << ma.getX() << ", ma.getY() = " << ma.getY() << endl;
  //resize(ma.getX(),ma.getY());
  m = new vector< vector<double> >();
  //(ma.getX(),ma.getY())
  (*m).resize(ma.getX());
  for ( int i=0; i<ma.getX(); i++ )
    (*m)[i].resize(ma.getY());
  x = ma.getX();
  y = ma.getY();
  for( int i=0 ; i<ma.getX() ; i++ )
    for( int j=0 ; j<ma.getY() ; j++ )
      (*m)[i][j] = ma[i][j];
  //cerr << "*** constructed Matrix_ripa object " << (int)this << " ***" << endl;

}


Matrix_ripa::~Matrix_ripa() {

  //cerr << "in Matrix_ripa::~Matrix_ripa()" << endl;
  //cerr << "*** deconstructing Matrix_ripa object " << (int)this << " ***" << endl;
  delete(m);

}


vector<double>& Matrix_ripa::operator[](int i) {

  if( i>x || i<0 )
    {
      //fprintf(stderr, "Matrix_ripa: illegal access on array of size %d / %d -- current cell accessed is: %d\n",x,y,i);
      Rcerr << "in Matrix_ripa::operator[]" << std::endl;
    }
  assert(m);
  //assert((*m)[i]);
  return (*m)[i];
}


void Matrix_ripa::operator=(Matrix_ripa& ma) {

  //cerr << "***** entering Matrix_ripa::Operator=() *****" << endl;
  if( x!=ma.getX() || y!=ma.getY() )
    resize( ma.getX(), ma.getY() );
  for( int i=0 ; i<ma.getX() ; i++ )
    for( int j=0 ; j<ma.getY() ; j++ )
      (*m)[i][j] = ma[i][j];
  //cerr << "***** leaving Matrix_ripa::operator=() *****" << endl;

}



void Matrix_ripa::removeRow( int row ) {

  // m[i].remove(row)...
  //info();
  for( unsigned int i=0; i<(*m).size(); i++ )
    {
      vector<double>::iterator it = ((*m)[i]).begin();
      it += row;
      (*m)[i].erase(it);
    }
  y--;

}


void Matrix_ripa::removeColumn( int column ) {

  //info();
  vector< vector<double> >::iterator it = (*m).begin();
  it += column;
  (*m).erase(it);
  x--;

}



void Matrix_ripa::resize( int nx, int ny ) {

  //cerr << "***** entering Matrix_ripa::resize() *****" << endl;
  if( m==NULL ) {
    m = new vector< vector<double> >();
  // (nx,ny)
    (*m).resize(nx);
    for ( int i=0; i<nx; i++ )
 	(*m)[i].resize(ny);
  }
  else
    {
      (*m).resize(nx);
      for ( int i=0; i<nx; i++ )
	(*m)[i].resize(ny);
    }
  x = nx;
  y = ny;
  //cerr << "***** leaving Matrix_ripa::resize() *****" << endl;

}

void Matrix_ripa::set( double d ) {

  for( int i=0; i<x; i++ )
    for( int j=0; j<y; j++ )
      (*m)[i][j] = d;

}

void Matrix_ripa::deb(void) {

  string s("Short hello from Matrix_ripa...");
  //fprintf(stderr,"%s\n", s.c_str());

}




Matrix_ripa& mapRtoC( double *img, int x, int y ) {

    Matrix_ripa& m = *(new Matrix_ripa(x,y));
    for( int i=0; i<x; i++ )
      for( int j=0; j<y; j++ )
	m[i][j] = img[ i*y + j ];
    return m;

}

void mapCtoR( Matrix_ripa & m, double *res ) {

  int x = m.getX();
  int y = m.getY();
  for( int i=0; i<x; i++ )
    for( int j=0; j<y; j++ )
      res[ i*y + j ] = m[i][j];

}




void Matrix_ripa::print(void) {

  Rcout << "\n ** Matrix_ripa is" << endl;
  for( int i=0; i<x; i++ )
    {
      for( int j=0; j<y; j++ )
	  Rcout << (*m)[i][j] << " ";
      Rcout << endl;
    }
  Rcout << "*********\n" << endl;

}

/*
Matrix_ripa::~Matrix_ripa() {

  info();
  //cout << "compared to this: m.size()="<<m.size()<<endl;
  //cout << "compared to this: m.size()="<<m.size()<<endl;
  vector< vector<double> >::iterator it = m.begin();
  cout << " m[0].size()=" << m[0].size() << endl;
  for( int i=0; i<m.size(); i++ )
    {
      //cout << " i=" << i << endl;
      //cout << " m[i].size()=" << m[i].size() << endl;
      //m.erase(it);
      m[i].clear();
      //cout << " m[i].size()=" << m[i].size() << endl;
      it--;
    }
  cout << "compared to this: m.size()="<<m.size()<<endl;

  m.clear();

}
*/
