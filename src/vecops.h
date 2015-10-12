/*----------------------------------------------------------------------
  File    : vecops.h
  Contents: some special vector operations
  Author  : Christian Borgelt
  History : 16.09.1996 file created
            04.02.1999 long int changed to int
            03.06.2001 function v_shuffle added
            02.01.2002 functions v_intsort, v_fltsort, v_dblsort added
            03.03.2002 functions v_reverse, v_intrev etc. added
            21.08.2003 function v_heapsort added
----------------------------------------------------------------------*/
#ifndef __VECOPS__
#define __VECOPS__

/*----------------------------------------------------------------------
  Type Definitions
----------------------------------------------------------------------*/
typedef int VCMPFN (const void *p1, const void *p2, void *data);

/*----------------------------------------------------------------------
  Functions
----------------------------------------------------------------------*/
extern void v_sort     (void *vec, int n, VCMPFN cmpfn, void *data);
extern void v_heapsort (void *vec, int n, VCMPFN cmpfn, void *data);
extern void v_move     (void *vec, int off, int n, int pos, int esz);
extern void v_shuffle  (void *vec, int n, double randfn (void));
extern void v_reverse  (void *vec, int n);

extern void v_intsort  (int    *vec, int n);
extern void v_intrev   (int    *vec, int n);

extern void v_fltsort  (float  *vec, int n);
extern void v_fltrev   (float  *vec, int n);

extern void v_dblsort  (double *vec, int n);
extern void v_dblrev   (double *vec, int n);
#endif
