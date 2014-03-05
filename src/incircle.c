/**

   Copyright (c) 2014 Talita Perciano
   complete license terms see file LICENSE_ripa

**/

#include	<stdio.h>
#include	<math.h>
#include        <stdlib.h>

void inCircle(int *x1, int *x2, int *y1, int *y2, double *image, int *nrow, int *ncol, double *region, int *regionLength){
	int x_1, x_2, y_1, y_2,row, col,i,j,count=0,enter=0,radius;
	double **img;
	
	x_1 = *x1;
	x_2 = *x2;
	y_1 = *y1;
	y_2 = *y2;
	
	row = *nrow; //y
	col = *ncol; //x
	
	
	radius = (int)sqrt((double)(pow((x_1 - x_2),2) + pow((y_1 - y_2),2)));
	
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
	
	count=0;
	for (i=0;i<row;i++){
		for (j=0;j<col;j++){
			if ( pow((j-x_1),2) + pow((i-y_1),2) > pow(radius,2)){
				img[i][j] = 1;
				enter = 1;
			}
			if (enter == 0){
				region[count] = img[i][j];
				count++;
			}
			enter = 0;
		}
	}
	
	*regionLength=count;
	count = 0;
	for (i=0;i<row;i++){
		for (j=0;j<col;j++){
			image[count] = img[i][j];
			count++;
		}
	}
	free(img);
}
