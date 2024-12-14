/*----------------------------------------------------------------------
  File    : tract.h
  Contents: item and transaction management
  Author  : Christian Borgelt
  History : 18.11.2001 file created from file apriori.c
            28.12.2001 first version completed
            02.01.2001 ta_sort mapped to v_intsort
            19.02.2002 transaction tree functions added
            17.07.2003 functions is_filter, ta_filter, tas_filter added
            21.08.2003 parameter 'heap' added to tas_sort, tat_create
            12.09.2003 function tas_total added
            20.09.2003 empty transactions in input made possible
            12/9/2013 fixed 64-bit address alignment (MFH)
            12/14/2024 moved to c11 variable array length in structs
----------------------------------------------------------------------*/
#ifndef __TRACT__
#define __TRACT__
#ifndef NIMAPFN
#define NIMAPFN
#endif
#include "vecops.h"
#include "symtab.h"
#include "tfscan.h"
#include "arch64.h"

/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
/* --- item appearance flags --- */
#define APP_NONE    0x00        /* item should be ignored */
#define APP_BODY    0x01        /* item may appear in rule body */
#define APP_HEAD    0x02        /* item may appear in rule head */
#define APP_BOTH    (APP_HEAD|APP_BODY)

/* --- error codes --- */
#define E_NONE        0         /* no error */
#define E_NOMEM     (-1)        /* not enough memory */
#define E_FOPEN     (-2)        /* cannot open file */
#define E_FREAD     (-3)        /* read error on file */
#define E_FWRITE    (-4)        /* write error on file */

#define E_ITEMEXP  (-16)        /* item expected */
#define E_DUPITEM  (-17)        /* duplicate item */
#define E_APPEXP   (-18)        /* appearance indicator expected */
#define E_UNKAPP   (-19)        /* unknown appearance indicator */
#define E_FLDCNT   (-20)        /* too many fields */

/*----------------------------------------------------------------------
  Type Definitions
----------------------------------------------------------------------*/
typedef struct {                /* --- an item --- */
  int     id;                   /* item identifier */
  int     frq;                  /* frequency in transactions */
  int     xfq;                  /* extended frequency (t.a. sizes) */
  int     app;                  /* appearance indicator */
} ITEM;                         /* (item) */

typedef struct {                /* --- a transaction --- */
  int     cnt;                  /* number of items */
  int     items[];              /* item identifier vector */
} TRACT;                        /* (transaction) */

typedef struct {                /* --- an itemset --- */
  TFSCAN  *tfscan;              /* table file scanner */
  char    chars[4];             /* special characters */
  NIMAP   *nimap;               /* name/identifier map */
  int     app;                  /* default appearance indicator */
  int     vsz;                  /* size of transaction buffer */
  int     cnt;                  /* number of items in transaction */
  int     *items;               /* items in transaction */
} ITEMSET;                      /* (item set) */

typedef struct {                /* --- a transaction set --- */
  ITEMSET *itemset;             /* underlying item set */
  int     max;                  /* maximum number of items per t.a. */
  int     vsz;                  /* size of transaction vector */
  int     cnt;                  /* number of transactions */
  int     total;                /* total number of items */
  TRACT   **tracts;             /* transaction vector */
} TASET;                        /* (transaction set) */

typedef struct _tatree {        /* --- a transaction tree (node) --- */
  int     cnt;                  /* number of transactions */
  int     max;                  /* size of largest transaction */
  int     size;                 /* node size (number of children) */
  int     items[];             /* next items in rep. transactions */
  /* followed by size pointers (need to be aligned on 64 bit arch) */ 
} TATREE;                       /* (transaction tree) */

/*----------------------------------------------------------------------
  Item Set Functions
----------------------------------------------------------------------*/
extern ITEMSET*    is_create  (void);
extern void        is_delete  (ITEMSET *iset);
extern TFSCAN*     is_tfscan  (ITEMSET *iset);
extern void        is_chars   (ITEMSET *iset, const char *blanks,
                                              const char *fldseps,
                                              const char *recseps,
                                              const char *cominds);

extern int         is_cnt     (ITEMSET *iset);
extern int         is_item    (ITEMSET *iset, const char *name);
extern const char* is_name    (ITEMSET *iset, int item);
extern int         is_getfrq  (ITEMSET *iset, int item);
extern int         is_setfrq  (ITEMSET *iset, int item, int frq);
extern int         is_addfrq  (ITEMSET *iset, int item, int frq);
extern int         is_getapp  (ITEMSET *iset, int item);
extern int         is_setapp  (ITEMSET *iset, int item, int app);

extern int         is_readapp (ITEMSET *iset, FILE *file);
extern int         is_read    (ITEMSET *iset, FILE *file);

extern int         is_recode  (ITEMSET *iset, int minfrq, int dir,
                               int *map,  int mode, int fullS);
extern int         is_filter  (ITEMSET *iset, const char *marks);
extern int         is_tsize   (ITEMSET *iset);
extern int*        is_tract   (ITEMSET *iset);

/*----------------------------------------------------------------------
  Transaction Functions
----------------------------------------------------------------------*/
extern void        ta_sort    (int *items, int n);
extern int         ta_unique  (int *items, int n);
extern int         ta_filter  (int *items, int n, const char *marks);

/*----------------------------------------------------------------------
  Transaction Set Functions
----------------------------------------------------------------------*/
extern TASET*      tas_create  (ITEMSET *itemset);
extern void        tas_delete  (TASET *taset, int delis);
extern ITEMSET*    tas_itemset (TASET *taset);

extern int         tas_cnt     (TASET *taset);
extern int         tas_add     (TASET *taset, const int *items, int n);
extern int*        tas_tract   (TASET *taset, int index);
extern int         tas_tsize   (TASET *taset, int index);
extern int         tas_total   (TASET *taset);

extern void        tas_recode  (TASET *taset, int *map, int cnt);
extern int         tas_filter  (TASET *taset, const char *marks);
extern void        tas_shuffle (TASET *taset, double randfn(void));
extern void        tas_sort    (TASET *taset, int heap);
extern int         tas_occur   (TASET *taset, const int *items, int n);

#ifndef NDEBUG
extern void        tas_show    (TASET *taset);
#endif

/*----------------------------------------------------------------------
  Transaction Tree Functions
----------------------------------------------------------------------*/
extern TATREE*     tat_create  (TASET *taset, int heap);
extern void        tat_delete  (TATREE *tat);
extern int         tat_cnt     (TATREE *tat);
extern int         tat_max     (TATREE *tat);
extern int         tat_size    (TATREE *tat);
extern int*        tat_items   (TATREE *tat);
extern int         tat_item    (TATREE *tat, int index);
extern TATREE*     tat_child   (TATREE *tat, int index);

#ifndef NDEBUG
extern void        tat_show    (TATREE *tat);
#endif

/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
#define is_tfscan(s)      ((s)->tfscan)

#define is_cnt(s)         nim_cnt((s)->nimap)
#define is_name(s,i)      nim_name(nim_byid((s)->nimap, i))
#define is_getfrq(s,i)    (((ITEM*)nim_byid((s)->nimap, i))->frq)
#define is_setfrq(s,i,f)  (((ITEM*)nim_byid((s)->nimap, i))->frq  = (f))
#define is_addfrq(s,i,f)  (((ITEM*)nim_byid((s)->nimap, i))->frq += (f))
#define is_getapp(s,i)    (((ITEM*)nim_byid((s)->nimap, i))->app)
#define is_setapp(s,i,a)  (((ITEM*)nim_byid((s)->nimap, i))->app  = (a))

#define is_tsize(s)       ((s)->cnt)
#define is_tract(s)       ((s)->items)

/*--------------------------------------------------------------------*/
#define ta_sort(v,n)      v_intsort(v,n)

/*--------------------------------------------------------------------*/
#define tas_itemset(s)    ((s)->itemset)
#define tas_cnt(s)        ((s)->cnt)
#define tas_max(s)        ((s)->max)

#define tas_tract(s,i)    ((s)->tracts[i]->items)
#define tas_tsize(s,i)    ((s)->tracts[i]->cnt)
#define tas_total(s)      ((s)->total)

#define tas_shuffle(s,f)  v_shuffle((s)->tracts, (s)->cnt, f)

/*--------------------------------------------------------------------*/
#define tat_cnt(t)        ((t)->cnt)
#define tat_max(t)        ((t)->max)
#define tat_size(t)       ((t)->size)
#define tat_item(t,i)     ((t)->items[i])
#define tat_items(t)      ((t)->items)

/* 64 bit alignment (MFH) */
#ifdef ARCH64                 /* adapt to even item number */
#define tat_align(n) ((n & 1) ? n : (n+1))  /* start is odd! n  needs to be odd */ 
                                            /* so that pointer addresses aligned */
#define tat_vec(t)    (((TATREE**)((t)->items + (((t)->size & 1) ? (t)->size : ((t)->size + 1)))))
#else
#define tat_align(n) (n)                    /* on 32 bit systems it is fine */
#define tat_vec(t)    ((TATREE**)((t)->items + (t)->size))
#endif  

#endif