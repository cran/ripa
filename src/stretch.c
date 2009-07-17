#include <stdio.h>
#include <stdlib.h>

/*
LINEAR STRETCH
*/

/*
Function to apply the linear stretch
a -the minimum value a pixel can take
b -the maximum value a pixel can take
c -the 5% value of the image pixels
d -the 95% value of the image pixels
nrow - number of rows
ncol - number of columns
*/
void stretch(double *band, double *a, double *b, double *c, double *d, int *nrow, int *ncol, double *out){
	int i=0,j=0,nr,nc,n;
	double e,f,g,h;
	nr=*nrow;
	nc=*ncol;
	e=*a;
	f=*b;
	g=*c;
	h=*d;
	n=nr*nc;
	if (g!=0 && h!=0){
		for (i=0;i<n;i++){
			out[i] = (band[i]-g)*((f-e)/(h-g))+e;
		}
		for (i=0;i<n;i++){
			if (out[i]>1) out[i] = 1;
			if (out[i]<0) out[i] = 0;
		}
	}
}
