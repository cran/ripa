#include <stdio.h>
#include <stdlib.h>

/*
MEDIAN FILTER
*/

#define SWAP(x, y) temp = (x); (x) = (y); (y) = temp

/*
A SIMPLE ALGORITHM THAT IMPLEMENTS QUICKSORT
*/        
void quicksort(double *sort, int low, int high){
	double temp;
	double pivot;
	int m;
	int i;
	if(low < high){
		SWAP(sort[low], sort[(high+low)/2]);
		pivot = sort[low];
		m = low;
		for (i = low + 1; i <= high; i++){
			if (sort[i] <  pivot) {
				m++;
				SWAP(sort[m], sort[i]);
			}
		}
		SWAP(sort[low], sort[m]);
		quicksort(sort, low, m - 1);
		quicksort(sort, m + 1, high);
	}
}

/*
IMPLEMENTATION OF THE MEDIAN FILTER

img - the image
nrow - number of rows
ncol - number of columns
lenMask - the length of the median filter mask
out - the out image
*/
void median(double *img, int *nrow, int *ncol, int *lenMask, double *out){
	int len, row, col,i,j,k,m=0,n=0,o,l,p=0,t;
	double **imgMatrix, *mask;
	l = *lenMask;
	row = *nrow;
	col = *ncol;
	k = (l-1)/2;
	
	imgMatrix = (double **)calloc(row,sizeof(double*));
	if (imgMatrix==NULL){
		 printf("\n\nMemory!");
		 exit(1);
	}
	for (i=0;i<row;i++){
		imgMatrix[i] = (double *)calloc(col,sizeof(double));
		if (imgMatrix[i]==NULL){
			printf("\n\nMemory!");
			exit(1);
		}
	}
	
	for (i=0;i<row;i++){
		for (j=0;j<col;j++){
			imgMatrix[i][j] = img[n];
			n++;
		}
	}
	mask = (double *)calloc((l*l),sizeof(double));
	if (mask==NULL){
		printf("\n\nMemory!");
		exit(1);
	}
	for (i=0;i<row;i++){
		for (j=0;j<col;j++){
			m=0;
			if ((i-k)<0 || (j-k)<0 || (i+k)>(row-1) || (j+k)>(col-1)){
				out[p] = 1;
				p++;
			}
			else if ((i-k)>=0 && (j-k)>=0 && (i+k)<=(row-1) && (j+k)<=(col-1)){
				for (n=-k;n<=k;n++){
					for (o=-k;o<=k;o++){
						mask[m] = imgMatrix[i+o][j+n];
						m++;
					}
				}
				quicksort(mask,0,(l*l)-1);
				out[p] = mask[((l*l)+1)/2];
				p++;
			}
		}
	}
	free(imgMatrix);
	free(mask);
}
 
