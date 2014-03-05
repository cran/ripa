/**

   Copyright (c) 2014 Talita Perciano
   complete license terms see file LICENSE_ripa

**/


#include <stdio.h>
#include <stdlib.h>
#include <Rcpp.h>
using namespace Rcpp;

/*
Save Lan images
*/

/*
The header of an LAN image
*/
struct header_type2{
	char descriptor[6];
	char ipack;
	int numbands;
	char blank1[4];
	int numcols;
	int numrows;
	int xstart;
	int ystart;
	char blank2[56];
	int maptype;
	int numclass;
	char blank3[10];
	float acre;
	float xmap;
	float ymap;
	float xcell;
	float ycell;
} header_lan2;


void writeLAN(char **filename1, char **filename2, int *in){
	FILE *fp;
	unsigned char l;
	int i,j,k=0,n,nrow,ncol,nbands,**matrix;
	if ((fp=fopen(*filename1,"rb"))==NULL){
		//printf("\nThe file could not be opened.\n");
        Rcerr << "in save::writeLAN()" << std::endl;
		return;
	}
	fread(&header_lan2,sizeof(struct header_type2),1,fp);
	fclose(fp);
	if ((fp=fopen(*filename2,"wb"))==NULL){
		//printf("\nThe file could not be opened.\n");
        Rcerr << "in lan::writeLAN()" << std::endl;
		return;
	}
	fwrite(&header_lan2,sizeof(struct header_type2),1,fp);
	
	nrow = header_lan2.numrows;
	ncol = header_lan2.numcols;
	n=nrow*ncol;
	nbands = header_lan2.numbands;
	
	matrix = (int**)calloc(n,sizeof(int *));
	if (matrix==NULL) Rcerr << "in save::writeLAN()" << std::endl;
	
	for (i=0;i<n;i++){
		matrix[i] = (int*)calloc(nbands,sizeof(int));
		if (matrix[i]==NULL) Rcerr << "in save::writeLAN()" << std::endl;
	}
	
	k=0;
	for (i=0;i<n;i++){
		for (j=0;j<nbands;j++){
			matrix[i][j] = in[k];
			k++;
		}
	}
	for (i=0;i<nrow;i++){
		for (j=0;j<nbands;j++){
			for (k=(i*ncol);k<(i*ncol+ncol);k++){
				l = matrix[k][j];
				fwrite(&l,sizeof(unsigned char),1,fp);
			}
		}
	}
	fclose(fp);
	free(matrix);
}
