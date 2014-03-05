/**

   Copyright (c) 2014 Talita Perciano
   complete license terms see file LICENSE_ripa

**/


#include	<stdio.h>

void normalize(double *img, int *row, int *col, float *bri){
	int nrow,ncol,i;
	float brit;
	
	nrow = *row;
	ncol = *col;
	brit = *bri;
	
	for (i=0;i<(nrow*ncol);i++){
		if (img[i] > 1) img[i] = 1;
	}
	if (brit<0){
		for (i=0;i<(nrow*ncol);i++){
			if (img[i] < 0) img[i] = 0;
		}
	}
	
}
