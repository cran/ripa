/*
This code is described in "Computational Geometry in C" (Second Edition),
Chapter 7.  It is not written to be comprehensible without the 
explanation in that book.

This program reads a polygon P followed by query points from stdin.
The input format is:
	n
	x0 y0
	x1 y1 
	...
	xn-1 yn-1
	qx qy
	qx qy
	qx qy
	...
For each query point q, InPoly returns one of four char's:
	i : q is strictly interior to P
	o : q is strictly exterior to P
	v : q is a vertex of P
	e : q lies on the relative interior of an edge of P
These represent mutually exclusive categories.
For an explanation of the code, see Chapter 7 of 
"Computational Geometry in C (Second Edition)."

Written by Joseph O'Rourke, contributions by Min Xu, June 1997.
Questions to orourke@cs.smith.edu.
--------------------------------------------------------------------
This code is Copyright 1998 by Joseph O'Rourke.  It may be freely 
redistributed in its entirety provided that this copyright notice is 
not removed.
--------------------------------------------------------------------
*/
#include        <stdlib.h>
#include	<stdio.h>
#include	<math.h>
#define	X	0
#define	Y	1
typedef	enum { FALSE, TRUE }	bool;

#define DIM     2               /* Dimension of points */
typedef int     tPointi[DIM];   /* type integer point */
typedef double  tPointd[DIM];   /* type double point */
#define PMAX    10000           /* Max # of pts in polygon */
int nvertices = 0;
   
typedef tPointi tPolygoni[PMAX];/* type integer polygon */

char	InPoly( tPointi q, tPolygoni P, int n );
void	PrintPoly( int n, tPolygoni P );
void	PrintPoint( tPointi p );
void    inpolyCopy ( tPolygoni a, tPolygoni b , int n );
int ReadPoly( tPolygoni P );

inpolyMain(int *nv, int *xvector, int *yvector, double *image, int *nrow, int *ncol, double *region, int *regionLength)
{
  int n,i,row,col,j,count=0,enter=0;
  double **img;
  tPolygoni P, Porig;
  tPointi q;
  char letter;
  
  row = *nrow;  //y
  col = *ncol;  //x
  
  img = (double **)calloc(row,sizeof(double *));
  for (i=0;i<row;i++){
    img[i] = (double *)calloc(col,sizeof(double));
  }
  
  for (i=0;i<row;i++){
    for (j=0;j<col;j++){
      img[i][j] = image[count];
      count++;
    }
  }
  
  nvertices = *nv;
  
  //printf( "Polygon:\n" );
  //printf( "   i   x   y\n");
  for ( i = 0; i < nvertices; i++ ) {
     P[i][0] = xvector[i];
    P[i][1] = yvector[i];
    //printf("%3d%4d%4d\n", i, P[i][0], P[i][1]);
  }
  //printf("n = %3d vertices read\n",n);
  putchar('\n');
  
  n = ReadPoly( P );
  inpolyCopy( P, Porig, n );

  //AQUI ENTRA UM FOR PARA TESTAR TODOS OS PONTOS DA IMAGEM
  count = 0;
  for (i=0;i<row;i++){
    for (j=0;j<col;j++){
      q[X] = j;
      q[Y] = i;
      letter = InPoly(q,P,n);
      if (letter=='o'){
         img[i][j]=1;
	 enter=1;
      }
      if (enter==0){
         region[count]=img[i][j];
	 count++;
      }
      enter=0;
      inpolyCopy( Porig, P, n );
    }
  }
  *regionLength=count;
  /*while( scanf( "%d %d", &q[X], &q[Y]) != EOF ) {
    printf( "InPoly (%3d, %3d) = %c\n", q[X], q[Y], InPoly( q, P, n ) );
     Refill the destroyed polygon with original. 
    inpolyCopy( Porig, P, n );       
  }*/
  count = 0;
  for (i=0;i<row;i++){
    for (j=0;j<col;j++){
      image[count] = img[i][j];
      count++;
    }
  }
  free(img);
}

/*
InPoly returns a char in {i,o,v,e}.  See above for definitions.
*/
char InPoly( tPointi q, tPolygoni P, int n )
{
  int	 i, i1;      /* point index; i1 = i-1 mod n */
  int	 d;          /* dimension index */
  double x;          /* x intersection of e with ray */
  int	 Rcross = 0; /* number of right edge/ray crossings */
  int    Lcross = 0; /* number of left edge/ray crossings */

  //printf("\n==>InPoly: q = "); PrintPoint(q); putchar('\n');
  
  /* Shift so that q is the origin. Note this destroys the polygon.
     This is done for pedogical clarity. */
  for( i = 0; i < n; i++ ) {
    for( d = 0; d < DIM; d++ )
      P[i][d] = P[i][d] - q[d];
  }
	
  /* For each edge e=(i-1,i), see if crosses ray. */
  for( i = 0; i < n; i++ ) {
    /* First see if q=(0,0) is a vertex. */
    if ( P[i][X]==0 && P[i][Y]==0 ) return 'v';
    i1 = ( i + n - 1 ) % n;
    //printf("e=(%d,%d)\t", i1, i);
    
    /* if e "straddles" the x-axis... */
    /* The commented-out statement is logically equivalent to the one 
       following. */
    /* if( ( ( P[i][Y] > 0 ) && ( P[i1][Y] <= 0 ) ) ||
       ( ( P[i1][Y] > 0 ) && ( P[i] [Y] <= 0 ) ) ) { */
    
    if( ( P[i][Y] > 0 ) != ( P[i1][Y] > 0 ) ) {
      
      /* e straddles ray, so compute intersection with ray. */
      x = (P[i][X] * (double)P[i1][Y] - P[i1][X] * (double)P[i][Y])
	/ (double)(P[i1][Y] - P[i][Y]);
      //printf("straddles: x = %g\t", x);
      
      /* crosses ray if strictly positive intersection. */
      if (x > 0) Rcross++;
    }
    //printf("Right cross=%d\t", Rcross);
    
    /* if e straddles the x-axis when reversed... */
    /* if( ( ( P[i] [Y] < 0 ) && ( P[i1][Y] >= 0 ) ) ||
       ( ( P[i1][Y] < 0 ) && ( P[i] [Y] >= 0 ) ) )  { */
    
    if ( ( P[i][Y] < 0 ) != ( P[i1][Y] < 0 ) ) { 
      
      /* e straddles ray, so compute intersection with ray. */
      x = (P[i][X] * (double)P[i1][Y] - P[i1][X] * (double)P[i][Y])
          / (double)(P[i1][Y] - P[i][Y]);
      //printf("straddles: x = %g\t", x);

      /* crosses ray if strictly positive intersection. */
      if (x < 0) Lcross++;
    }
    //printf("Left cross=%d\n", Lcross);
  }	
  
  /* q on the edge if left and right cross are not the same parity. */
  if( ( Rcross % 2 ) != (Lcross % 2 ) )
    return 'e';
  
  /* q inside iff an odd number of crossings. */
  if( (Rcross % 2) == 1 )
    return 'i';
  else	return 'o';
}

void PrintPoint( tPointi p )
{
  int	i;
  
  putchar('(');
  for ( i = 0; i < DIM; i++ ) {
    //printf("%d", p[i]);
    if ( i != DIM-1 ) putchar(',');
  }
  putchar(')');
}

/*
   Reads in the coordinates of the vertices of a polygon from stdin,
   puts them into P, and returns n, the number of vertices.
   Formatting conventions: etc.
   */
int ReadPoly( tPolygoni P )
{
  int	i, n = nvertices;
  
  do {
    if ( n < PMAX )
      break;
    printf("Error in read_poly:  too many points; max is %d\n", PMAX);
  }
  while ( 1 );

  return n;
}

void PrintPoly( int n, tPolygoni P )
{
  int	i;
  
  //printf("Polygon:\n");
  //printf("  i   x   y\n");
  //for( i = 0; i < n; i++ )
    //printf("%3d%4d%4d\n", i, P[i][0], P[i][1]);
}

/* Copy polygon a to b (overwriting b). */
void inpolyCopy( tPolygoni a, tPolygoni b, int n )
{
  int i, j;
  
  for ( i=0; i < n; i++)
    for ( j = 0; j < DIM; j++ )
      b[i][j] = a[i][j];
}

bool EqPoint( tPointi a, tPointi b )
{
  int     i;
  for ( i = 0; i < DIM; i++ )
    if ( a[i] != b[i])
      return  FALSE;
  return  TRUE;
}

