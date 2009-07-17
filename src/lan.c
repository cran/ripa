#include <stdio.h>
#include <stdlib.h>

/*
READING OF LAN IMAGES
*/

/*
The header of an LAN image
*/
struct header_type{
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
} header;

void getImgData(char **filename, int *numbands, int *row, int *col){
	FILE *fp;
	if ((fp=fopen(*filename,"rb"))==NULL){
		printf("\nThe file could not be opened.\n");
		return;
	}
	fread(&header,sizeof(struct header_type),1,fp);
	*numbands = header.numbands;
	*row = header.numrows;
	*col = header.numcols;
	fclose(fp);
}

/*
Function to read the LAN file
*/
void readLAN(char **filename, int *out){
	FILE *fp;
	unsigned char l;
	int i,j,k,ncol,nrow,nbands,n,**matrix;
	if ((fp=fopen(*filename,"rb"))==NULL){
		printf("\nThe file could not be opened.\n");
		return;
	}
	fread(&header,sizeof(struct header_type),1,fp);
	
	
	nrow = header.numrows;
	ncol = header.numcols;
	n = nrow*ncol;
	nbands = header.numbands;
	

	matrix = (int**)calloc(n,sizeof(int *));
	if (matrix==NULL) exit(1);

	for (i=0;i<n;i++){
		matrix[i] = (int*)calloc(nbands,sizeof(int));
		if (matrix[i]==NULL) exit(1);
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
