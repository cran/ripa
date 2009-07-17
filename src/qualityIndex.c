#include <stdio.h>
#include <stdlib.h>

/*
QUALITY INDEX
*/

/*
Function that calculates the index of each window
meanx - mean of the values of the original image
meany - mean of the values of the transformed image
varx - variance of the values of the original image
vary - variance of the values of the transformed image
sdxy - standard deviation of the two images
*/
double calcQ(double meanx,double meany, double varx, double vary,double sdxy){
     double q;
     
     q = (4*sdxy*meanx*meany)/((varx+vary)*((meanx*meanx)+(meany*meany)));
     return(q);
}

/*
Function that calculates the mean of the window
B - length of the window
*/
double mean(double **window,int B){
	int i,j;
	double sum=0;
	for (i=0;i<B;i++){
		for (j=0;j<B;j++){
			sum = sum + window[i][j];
		}	
	}
	return(sum/(B*B));
}

/*
Function to calculate the variance of the window
mean - the mean
*/
double var(double **window,int B,double mean){
	int i,j;
	double sum=0;
	for (i=0;i<B;i++){
		for (j=0;j<B;j++){
			sum = sum + (window[i][j]-mean)*(window[i][j]-mean);
		}
	}
	return(sum/(B*B-1));
}

/*
Function to calculate the standard deviation of the two images
*/
double sdxy(double **window_x,double **window_y,int B,double meanx,double meany){
	int i,j;
	double sum=0;
	for (i=0;i<B;i++){
		for (j=0;j<B;j++){
			sum = sum + (window_x[i][j]-meanx)*(window_y[i][j]-meany);
		}
	}
	return(sum/(B*B-1));
}

/*
Function that calculates the quality index
*/
void quality(double *image_x,double *image_y, int *ncol, int *nrow, int *tamWindow, double *out){
     int i,j,k=0,l,indexr,indexc_x,indexc_y,M=0,len,row,col,B;
     double **window_x,**window_y,*q, **img_x,**img_y,Mean,sum=0,mx,my,vx,vy,stdxy;
     row = *nrow;
     col = *ncol;
     B = *tamWindow;
     
     len = (row-B)*(col-B);
     
     q = (double *)calloc(len,sizeof(double));
     if (q==NULL) exit(1);
     
     window_x = (double **)calloc(B,sizeof(double*));
     if (window_x==NULL) exit(1);
     for (i=0;i<B;i++){
     	window_x[i] = (double *)calloc(B,sizeof(double));
	if (window_x[i]==NULL) exit(1);
     }
     window_y = (double **)calloc(B,sizeof(double*));
     if (window_y==NULL) exit(1);
     for (i=0;i<B;i++){
     	window_y[i] = (double *)calloc(B,sizeof(double));
	if (window_y[i]==NULL) exit(1);
     }
     
     img_x = (double **)calloc(row,sizeof(double *));
     if (img_x==NULL) exit(1);
     for (i=0;i<row;i++){
     	img_x[i] = (double *)calloc(col,sizeof(double));
	if (img_x[i]==NULL) exit(1);
     }
     img_y = (double **)calloc(row,sizeof(double *));
     if (img_y==NULL) exit(1);
     for (i=0;i<row;i++){
     	img_y[i] = (double *)calloc(col,sizeof(double));
	if (img_y[i]==NULL) exit(1);
     }
     
     for (i=0;i<row;i++){
	 for (j=0;j<col;j++){
             img_x[i][j] = image_x[k];
             k++;
         }
     }   
     k =0;
     
     for (i=0;i<row;i++){
	 for (j=0;j<col;j++){
             img_y[i][j] = image_y[k];
             k++;
         }
     }
     
     indexr = 0;
     for (i=0;i<row-B;i++){
         for (j=0;j<col-B;j++){
             for (k=i;k<B+i;k++){
                 indexc_x = 0;
                 indexc_y = 0;
		 for (l=j;l<B+j;l++){
                     window_x[indexr][indexc_x]=img_x[k][l];
		     indexc_x++;
                 }
		 for (l=j;l<B+j;l++){
                     window_y[indexr][indexc_y]=img_y[k][l];
                     indexc_y++;
                 }
                 indexr++;
             }
	     indexr = 0;
	     
             mx = mean(window_x,B);
	     my = mean(window_y,B);
	     vx = var(window_x,B,mx);
	     vy = var(window_y,B,my);
	     stdxy = sdxy(window_x,window_y,B,mx,my);
	         
	     q[M]=calcQ(mx,my,vx,vy,stdxy);
	     M++;
         }
     }
     
     for (i=0;i<len;i++){
     	sum = sum + q[i];
     }
     Mean = sum/len;
     out[0] = Mean;
     free(window_x);
     free(window_y);
     free(img_x);
     free(img_y);
     free(q);
}
