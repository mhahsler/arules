/*----------------------------------------------------------------------
  File    : bitmat.h
  Contents: bit matrix management
  Author  : Christian Borgelt
  History : 09.06.2002 file created
            10.12.2002 result report function extended
            18.08.2003 memory benchmarking functionality added
            20.08.2003 closed/maximal item set search added
            22.08.2003 function bm_setcol added
            11.09.2003 sparse matrix representation added
----------------------------------------------------------------------*/
#ifndef __BITMAT__
#define __BITMAT__
#include <limits.h>

/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
#define BM_SHIFT     5          /* bit shift for 32 bit integers */
#define BM_MASK   0x1f          /* bit mask  for 32 bit integers */

/* --- search modes --- */
#define BM_NORMAL    0          /* all frequent item sets */
#define BM_CLOSED    1          /* closed  item sets */
#define BM_MAXIMAL   2          /* maximal item sets */
#define BM_GENERATOR 3          /* generator item sets */
/*----------------------------------------------------------------------
  Type Definitions
----------------------------------------------------------------------*/
typedef struct {                /* --- a bit matrix --- */
  int sparse;                   /* flag for sparse representation */
  int rowvsz, colvsz;           /* vector sizes */
  int rowcnt, colcnt;           /* number of columns/rows */
  int **rows;                   /* array of row bit vectors */
  int *buf;                     /* buffer for intersection */
  int *supps;                   /* supports of represented sets */
  #ifdef BENCH
  int mem;                      /* memory used during search */
  #endif
} BITMAT;                       /* (bit matrix) */

typedef void BMREPFN (int *ids, int cnt, int supp, int *tal,void *data);
                                /* report function */

/*----------------------------------------------------------------------
  Functions
----------------------------------------------------------------------*/
extern BITMAT* bm_create (int rowcnt, int colcnt, int sparse);
extern void    bm_delete (BITMAT *bm);
extern int     bm_resize (BITMAT *bm, int rowcnt, int colcnt);
extern int     bm_reduce (BITMAT *bm, const int* map);

extern int     bm_sparse (BITMAT *bm);
extern int     bm_rowcnt (BITMAT *bm);
extern int     bm_colcnt (BITMAT *bm);
extern int     bm_set    (BITMAT *bm, int row, int col);
extern int     bm_clr    (BITMAT *bm, int row, int col);
extern int     bm_get    (BITMAT *bm, int row, int col);
extern void    bm_setcol (BITMAT *bm, int col, const int *ids, int n);
extern int     bm_addcol (BITMAT *bm,          const int *ids, int n);

extern int     bm_count  (BITMAT *bm, int row);
extern int     bm_ones   (BITMAT *bm, int row);
extern int     bm_allone (BITMAT *bm, int mode, int supp,
                          int min, int max, BMREPFN report, void *data,int tacnt);

#ifndef NDEBUG
extern void    bm_show   (BITMAT *bm, FILE *file, int transpose);
#endif

/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
#define bm_sparse(m)     ((m)->sparse)
#define bm_rowcnt(m)     ((m)->rowcnt)
#define bm_colcnt(m)     ((m)->colcnt)
#define bm_set(m,r,c)    ((m)->rows[r][((c) >> BM_SHIFT)] |= \
                            1 << ((c) & BM_MASK))
#define bm_clr(m,r,c)    ((m)->rows[r][((c) >> BM_SHIFT)] &= \
                            1 << ((c) & BM_MASK))
#define bm_get(m,r,c)    ((m)->rows[r][((c) >> BM_SHIFT)] &  \
                           (1 << ((c) & BM_MASK)))
#define bm_ones(m,r)     (*((m)->rows[r] -1))
#endif
