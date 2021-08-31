/*----------------------------------------------------------------------
  File    : istree.h
  Contents: item set tree management
  Author  : Christian Borgelt
  History : 22.01.1996 file created
            29.01.1996 ISNODE.offset and ISNODE.id added
            08.02.1996 ISTREE.tacnt, ISTREE.curr, ISTREE.index,
                       ISTREE.head and ISTREE.conf added
            28.03.1996 support made relative to number of item sets
            23.11.1996 ISTREE.levels (first nodes of each level) added
            24.11.1996 ISTREE.arem (add. rule evaluation measure) added
            18.08.1997 chi^2 evaluation measure added
                       parameter 'minlen' added to function ist_init()
            11.02.1998 parameter 'minval' added to function ist_init()
            14.05.1998 item set tree navigation functions added
            08.08.1998 parameter 'apps' added to function ist_create()
            20.08.1998 structure ISNODE redesigned
            07.09.1998 function ist_hedge added
            08.12.1998 function ist_gettac added,
                       float changed to double
            05.02.1999 long int changed to int
            26.08.1999 functions ist_first and ist_last added
            05.11.1999 rule evaluation measure EM_AIMP added
            08.11.1999 parameter 'aval' added to function ist_rule
            01.04.2001 functions ist_set and ist_getcntx added
            28.12.2001 sort function moved to module tract
            07.02.2002 function ist_clear removed, ist_settac added
            11.02.2002 optional use of identifier maps in nodes added
            12.02.2002 ist_first and ist_last replaced by ist_next
            12.03.2003 parameter lift added to function ist_rule
            17.07.2003 functions ist_itemcnt and ist_check added
            18.07.2003 function ist_maxfrq added (item set filter)
            11.08.2003 item set filtering generalized (ist_filter)
            09.05.2004 parameter 'aval' added to function ist_set
            12/9/2013 fixed 64-bit address alignment (MFH)
----------------------------------------------------------------------*/
#ifndef __ISTREE__
#define __ISTREE__
#include "tract.h"
/*#undef ARCH64 */

/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
/* --- additional evaluation measures --- */
#define EM_NONE     0           /* no measure */
#define EM_DIFF     1           /* absolute conf. difference to prior */
#define EM_LOGQ     1           /* logarithm of support quotient */
#define EM_QUOT     2           /* difference of conf. quotient to 1 */
#define EM_AIMP     3           /* abs. diff. of improvement to 1 */
#define EM_INFO     4           /* information difference to prior */
#define EM_CHI2     5           /* normalized chi^2 measure */
#define EM_UNKNOWN  6           /* unknown measure */

/* --- item appearances --- */
#define IST_IGNORE  0           /* ignore item */
#define IST_BODY    1           /* item may appear in rule body */
#define IST_HEAD    2           /* item may appear in rule head */
#define IST_BOTH    (IST_HEAD|IST_BODY)

/* --- item set filter modes --- */
#define IST_MAXFRQ  0           /* maximally frequent item sets */
#define IST_GENERATOR  1           /* generator item sets */
#define IST_CLOSED  2           /* closed item sets */
/*----------------------------------------------------------------------
  Type Definitions
----------------------------------------------------------------------*/
typedef struct _isnode {        /* --- item set node --- */
  struct _isnode *parent;       /* parent node */
  struct _isnode *succ;         /* successor node on same level */
  int            id;            /* identifier used in parent node */
  int            chcnt;         /* number of child nodes */
  int            size;          /* size   of counter vector */
  int            offset;        /* offset of counter vector */
  int            cnts[1];       /* counter vector */
  /* has size int cnts entries followed by chcnt ISNODE* */
} ISNODE;                       /* (item set node) */

typedef struct {                /* --- item set tree --- */
  int    tacnt;                 /* number of transactions counted */
  int    lvlvsz;                /* size of level vector */
  int    lvlcnt;                /* number of levels (tree height) */
  int    rsdef;                 /* rule support definition */
  int    arem;                  /* additional rule evaluation measure */
  int    size;                  /* size of item set/rule/hyperedge */
  int    index;                 /* index in item set node */
  int    plen;                  /* current path length */
  int    item;                  /* head item of previous rule */
  int    hdonly;                /* head only item in current set */
  ISNODE **levels;              /* first node of each level */
  double supp;                  /* minimal support of an item set */
  double conf;                  /* minimal confidence of a rule */
  double minval;                /* minimal evaluation measure value */
  ISNODE *curr;                 /* current node for traversal */
  ISNODE *node;                 /* item set node for extraction */
  ISNODE *head;                 /* head item node for extraction */
  int    *buf;                  /* buffer for paths (support check) */
  int    *path;                 /* current path / (partial) item set */
  int    *map;                  /* to create identifier maps */
  int    memopt;                /* whether to optimize memory usage */
#ifdef BENCH                    /* if benchmark version */
  int    cnt;                   /* number of counters */
  int    nec;                   /* number of necessary counters */
  int    chcnt;                 /* number of child pointers */
  int    chnec;                 /* number of necessary child pointers */
  int    bytes;                 /* number of bytes used */
#endif
  char   apps[1];               /* item appearances, this will be 
                                    allocated as an array */
} ISTREE;                       /* (item set tree) */

/*----------------------------------------------------------------------
  Functions
----------------------------------------------------------------------*/
extern ISTREE* ist_create  (int itemcnt, double supp, double conf,
                            int rsdef, const char *apps, int memopt);
extern void    ist_delete  (ISTREE *ist);
extern int     ist_itemcnt (ISTREE *ist);

extern void    ist_count   (ISTREE *ist, int *set, int cnt);
extern void    ist_countx  (ISTREE *ist, TATREE *tat);
extern int     ist_settac  (ISTREE *ist, int cnt);
extern int     ist_gettac  (ISTREE *ist);
extern int     ist_check   (ISTREE *ist, char *marks);
extern int     ist_addlvl  (ISTREE *ist);
extern int     ist_height  (ISTREE *ist);

extern void    ist_up      (ISTREE *ist, int root);
extern int     ist_down    (ISTREE *ist, int item);
extern int     ist_next    (ISTREE *ist, int item);
extern void    ist_setcnt  (ISTREE *ist, int item, int cnt);
extern int     ist_getcnt  (ISTREE *ist, int item);
extern int     ist_getcntx (ISTREE *ist, int *set, int cnt);

extern void    ist_filter  (ISTREE *ist, int mode);
extern void    ist_init    (ISTREE *ist, int minlen,
                            int arem, double minval);
extern int     ist_set     (ISTREE *ist, int *set,
                            double *supp, double *aval);
extern int     ist_rule    (ISTREE *ist, int *rule,
                            double *supp, double *conf,
                            double *lift, double *aval);
extern int     ist_hedge   (ISTREE *ist, int *hedge,
                            double *supp, double *conf);

#ifndef NDEBUG
extern void    ist_show    (ISTREE *ist);
#endif

/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
#define ist_itemcnt(t)     ((t)->levels[0]->size)
#define ist_settac(t,n)    ((t)->tacnt = (n))
#define ist_gettac(t)      ((t)->tacnt)
#define ist_height(t)      ((t)->lvlcnt)

#ifdef ARCH64
#define get_vec(t)         ((ISNODE**) (t->cnts + (((t->size) & 1)?(t->size)+1:t->size)))
#else
#define get_vec(t)         ((ISNODE**) (t->cnts + t->size))
#endif
#endif
