/***************************************************
 *
 *  Laplacian Filter Functions for rimage (R package)
 *
 *  $Header: /database/repository/rimage/src/laplacian.c,v 1.3.2.1 2005/01/12 08:39:59 tomo Exp $
 *
 *  Copyright (c) 2003 Nikon Digital Technologies Co., Ltd.
 *  complete license terms see file LICENSE
 ***************************************************/

extern void clearFrame(double *img, long w, long h);

void laplacian(double *img, int *w, int *h, double *eimg) {
	int i, j;
	clearFrame(eimg, *w, *h);
	for (i = 1; i<*w-1; i++) {
		for (j = 1; j<*h-1; j++) {
			eimg[*h * i + j] = img[*h * i + (j-1)] + 
				img[*h * (i-1) + j] + 
				img[*h * (i+1) + j] + 
				img[*h * i + (j+1)] - 4.0 * img[*h * i + j];
		}
	}
}

/* the end of this file */

