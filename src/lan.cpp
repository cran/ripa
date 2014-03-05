/**

   Copyright (c) 2014 Talita Perciano
   complete license terms see file LICENSE

**/


#include <stdio.h>
#include <stdlib.h>
#include "lan.h"
#include <Rcpp.h>
using namespace Rcpp;

/*
READING OF LAN IMAGES
*/



/*
Function to read the LAN file
*/
extern "C" {
	void readLAN(char **filename, int *out){
		FILE *fp;
		unsigned char l;
		int i,j,k,ncol,nrow,nbands,n,**matrix;
		if ((fp=fopen(*filename,"rb"))==NULL){
			Rcerr << "The file could not be opened." << std::endl;
			Rcerr << "in lan::readLAN()" << std::endl;
			return;
		}
		fread(&header_lan,sizeof(struct header_type),1,fp);
	
	
		nrow = header_lan.numrows;
		ncol = header_lan.numcols;
		n = nrow*ncol;
		nbands = header_lan.numbands;
	

		matrix = (int**)calloc(n,sizeof(int *));
		if (matrix==NULL) {
			Rcerr << "in readLAN()" << std::endl;
			return;
		}

		for (i=0;i<n;i++){
			matrix[i] = (int*)calloc(nbands,sizeof(int));
			if (matrix[i]==NULL) {
				Rcerr << "in readLAN()" << std::endl;
				return;
			}
		}

		for (i=0;i<nrow;i++){
			for (j=0;j<nbands;j++){
				for (k=(i*ncol);k<(i*ncol+ncol);k++){
					fread(&l,sizeof(unsigned char),1,fp);
					matrix[k][j] = l;
				}
			}
		}
	
		k=0;
		for (i=0;i<n;i++){
			for (j=0;j<nbands;j++){
				out[k] = matrix[i][j];
				k++;
			}
		}
	
	
		fclose(fp);
		free(matrix);
	}
}


extern "C" {
	void getImgData(char **filename, int *numbands, int *row, int *col){
		FILE *fp;
		if ((fp=fopen(*filename,"rb"))==NULL){
			Rcerr << "The file could not be opened." << std::endl;
			Rcerr << "in lan::getImgData()" << std::endl;
			return;
		}
		fread(&header_lan,sizeof(struct header_type),1,fp);
		*numbands = header_lan.numbands;
		*row = header_lan.numrows;
		*col = header_lan.numcols;
		fclose(fp);
	}

}
