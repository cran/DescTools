 /* External and interal  API  of  C and Fortran routines in robustbase */

/* C code which includes this, typically includes <R.h> */

/* call via .C() from R : */
void wgt_himed_i(double *x, Sint *n,  Sint *iw, double *res);
void wgt_himed  (double *x, Sint *n, double *w, double *res);


double whimed_i(double *a, int *iw, int n,
		double *acand, double *a_srt, int *iw_cand);
double whimed(double *a, double *w, int n,
	      double *acand, double *a_srt, double *w_cand);
