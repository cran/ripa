/***************************************************
 *
 *  Sobel Filter Function for rimage (R package)
 *
 *  Copyright (c) 2003 Nikon Digital Technologies Co., Ltd.
 *  complete license terms see file LICENSE
 *

 ***************************************************/ 

void clearFrame(double *img, long w, long h) {
	int i, j;
	for (j = 0; j < w; j++) {
		img[j] = 0.0;
		img[(h-1)*w + j] = 0.0;
	}
	for (i = 0; i < h; i++) {
		img[w * i] = 0.0;
		img[w * i + (w-1)] = 0.0;
	}
}

void sobel_v(double *img, int *w, int *h, double *eimg) {
	int i, j;
	clearFrame(eimg, *w, *h);
	for (i = 1; i<*w-1; i++) {
		for (j = 1; j<*h-1; j++) {
			eimg[*h * i + j] = 
				- img[*h * (i-1) + (j-1)] 
				- 2.0 * img[*h * i + (j-1)]
				- img[*h * (i+1) + (j-1)] 
				+ img[*h * (i-1) + (j+1)] 
				+ 2.0 * img[*h * i + (j+1)]
				+ img[*h * (i+1) + (j+1)];
		}
	}
}

void sobel_h(double *img, int *w, int *h, double *eimg) {
	int i, j;
	clearFrame(eimg, *w, *h);
	for (i = 1; i<*w-1; i++) {
		for (j = 1; j<*h-1; j++) {
			eimg[*h * i + j] = 
				- img[*h * (i-1) + (j-1)] 
				- 2.0 * img[*h * (i-1) + j]
				- img[*h * (i-1) + (j+1)] 
				+ img[*h * (i+1) + (j-1)] 
				+ 2.0 * img[*h * (i+1) + j]
				+ img[*h * (i+1) + (j+1)];
		}
	}
}

/* the end of the file */
