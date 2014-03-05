/***************************************************
 *
 *  Smooth Filter Functions for rimage (R package)
 *
 * 
 *  $Header: /database/repository/rimage/src/smooth.c,v 1.5.2.3 2005/01/12 08:39:59 tomo Exp $
 *  Copyright (c) 2003 Nikon Digital Technologies Co., Ltd.
 *  complete license terms see file LICENSE
 * 
 ***************************************************/

#ifdef linux
#include <values.h>
#else
#include <float.h>
#define MINDOUBLE DBL_MIN
#define MAXDOUBLE DBL_MAX
#endif

extern void clearFrame(double *img, long w, long h);


double max(double *val, int len) {
	double w;
	int i;
	w = MINDOUBLE;
	for (i = 0; i < len; i++) if (val[i] > w) w = val[i];
	return w;
}


double min(double *val, int len) {
	double w;
	int i;
	w = MAXDOUBLE;
	for (i = 0; i < len; i++) if (val[i] < w) w = val[i];
	return w;
}

double mean(double *val, int len) {
	double w;
	int i;
	w = 0.0;
	for (i = 0; i < len; i++) w += val[i];
	return (w/(double)len);
}

void meanfilter(double *img, int *width, int *height, double *eimg) {
  int i, j;
  double sum=0.0;

  for (i = 1; i<*width-1; i++) {
    for (j = 1; j<*height-1; j++) {
      sum += img[*height * (i-1) + (j-1)];
      sum += img[*height * i + (j-1)];
      sum += img[*height * (i+1) + (j-1)];
      sum += img[*height * (i-1) + j];
      sum += img[*height * (i+1) + j];
      sum += img[*height * (i-1) + (j+1)];
      sum += img[*height * i + (j+1)];
      sum += img[*height * (i+1) + (j+1)];
      sum += img[*height * i + j];
      eimg[*height * i +j] = sum / 9.0;
      sum = 0.0;
    }
  }

}

void minfilter(double *img, int *w, int *h, double *eimg) {
	int i, j;
	double val[8];
	clearFrame(eimg, *w, *h);
	for (i = 1; i<*w-1; i++) {
		for (j = 1; j<*h-1; j++) {
			val[0] = img[*h * (i-1) + (j-1)];
			val[1] = img[*h * i + (j-1)];
			val[2] = img[*h * (i+1) + (j-1)];
			val[3] = img[*h * (i-1) + j];
			val[4] = img[*h * (i+1) + j];
			val[5] = img[*h * (i-1) + (j+1)];
			val[6] = img[*h * i + (j+1)];
			val[7] = img[*h * (i+1) + (j+1)];
			eimg[*h * i + j] = min(val, 8);
		}
	}
}

void maxfilter(double *img, int *w, int *h, double *eimg) {
	int i, j;
	double val[8];
	clearFrame(eimg, *w, *h);
	for (i = 1; i<*w-1; i++) {
		for (j = 1; j<*h-1; j++) {
			val[0] = img[*h * (i-1) + (j-1)];
			val[1] = img[*h * i + (j-1)];
			val[2] = img[*h * (i+1) + (j-1)];
			val[3] = img[*h * (i-1) + j];
			val[4] = img[*h * (i+1) + j];
			val[5] = img[*h * (i-1) + (j+1)];
			val[6] = img[*h * i + (j+1)];
			val[7] = img[*h * (i+1) + (j+1)];
			eimg[*h * i + j] = max(val, 8);
		}
	}
}

/* the end of this file */

