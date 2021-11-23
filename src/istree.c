/*----------------------------------------------------------------------
  File    : istree.c
  Contents: item set tree management
  Author  : Christian Borgelt
  History : 22.01.1996 file created
            07.02.1996 _child, _count, ist_addlvl, and ist_count
            09.02.1996 ist_rule programmed and debugged
            10.02.1996 empty rule bodies made optional
            28.03.1996 support made relative to number of item sets
            25.06.1996 function _count optimized
            23.11.1996 rule extraction redesigned
            24.11.1996 rule selection criteria added
            18.08.1997 normalized chi^2 measure added
                       parameter minlen added to function ist_init()
            15.01.1998 confidence comparison changed to >=
            23.01.1998 integer support computation changed (ceil)
            26.01.1998 condition added to set extension in _child
            10.02.1998 bug in computation of EM_INFO fixed
            11.02.1998 parameter 'minval' added to function ist_init()
            14.05.1998 item set tree navigation functions added
            08.08.1998 item appearances considered for rule selection
            20.08.1998 deferred child node vector allocation added
            02.09.1998 several assertions added
            05.09.1998 bug concerning node id fixed
            07.09.1998 function ist_hedge added
            22.09.1998 bug in rule extraction (item appearances) fixed
            23.09.1998 computation of chi^2 measure simplified
            05.02.1999 long int changed to int
            25.08.1999 rule extraction simplified
            05.11.1999 rule evaluation measure EM_AIMP added
            08.11.1999 parameter 'aval' added to function ist_rule
            11.11.1999 rule consequents moved to first field
            01.12.1999 bug in node reallocation fixed
            01.04.2001 functions ist_set and ist_getcntx added,
                       functions _count and _getsupp improved
            28.12.2001 sort function moved to module tract
            07.02.2002 tree clearing removed, counting improved
            08.02.2002 child creation improved (check of body support)
            10.02.2002 IST_IGNORE bugs fixed (ist_set and ist_hedge)
            11.02.2002 memory usage minimization option added
            12.02.2002 ist_first and ist_last replaced by ist_next
            19.02.2002 transaction tree functions added
            09.10.2002 bug in function ist_hedge fixed (conf. comp.)
            12.03.2003 parameter lift added to function ist_rule
            17.07.2003 check of item usage added (function ist_check)
            18.07.2003 maximally frequent item set filter added
            11.08.2003 item set filtering generalized (ist_filter)
            15.08.2003 renamed new to cur in ist_addlvl (C++ compat.)
            14.11.2003 definition of F_HDONLY changed to INT_MIN
            02.12.2003 skipping unnecessary subtrees added (_stskip)
            03.12.2003 bug in ist_check for rule mining fixed
            12.12.2003 padding for 64 bit architecture added
            09.05.2004 additional selection measure for sets added
            12/9/2013 fixed 64-bit address alignment (MFH)
----------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <float.h>
#include <math.h>
#include <assert.h>
#include "istree.h"
#ifdef STORAGE
#include "storage.h"
#endif

/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
#define LN_2       0.69314718055994530942   /* ln(2) */
#define EPSILON    1e-12        /* to cope with roundoff errors */
#define BLKSIZE    32           /* block size for level vector */
#define F_HDONLY   INT_MIN      /* flag for head only item in path */
#define F_SKIP     INT_MIN      /* flag for subtree skipping */
#define ID(n)      ((int)((n)->id & ~F_HDONLY))
#define HDONLY(n)  ((int)((n)->id &  F_HDONLY))

/*----------------------------------------------------------------------
  Type Definitions
----------------------------------------------------------------------*/
typedef double EVALFN (double head, double body, double post);
/* function to compute an additional evaluation measure */

/*----------------------------------------------------------------------
  Auxiliary Functions
----------------------------------------------------------------------*/

static int _bsearch (int *vec, int n, int id)
{                               /* --- binary search for an item */
  int i, k;                     /* left and middle index */

  assert(vec && (n > 0));       /* check the function arguments */
  for (i = 0; i < n; ) {        /* while the range is not empty */
    k = (i + n) >> 1;           /* get index of middle element */
    if      (vec[k] > id) n = k;
    else if (vec[k] < id) i = k+1;
    else return k;              /* adapt range boundaries or return */
  }                             /* the index the id. was found at */
  return -1;                    /* return 'not found' */
}  /* _bsearch() */

/*--------------------------------------------------------------------*/

static void _count (ISNODE *node, int *set, int cnt, int min)
{                               /* --- count transaction recursively */
  int    i;                     /* vector index */
  int    *map, n;               /* identifier map and its size */
  ISNODE **vec;                 /* child node vector */

  assert(node                   /* check the function arguments */
      && (cnt >= 0) && (set || (cnt <= 0)));
  if (node->offset >= 0) {      /* if a pure vector is used */
    if (node->chcnt == 0) {     /* if this is a new node */
      n = node->offset;         /* get the index offset */
      while ((cnt > 0) && (*set < n)) {
        cnt--; set++; }         /* skip items before first counter */
      while (--cnt >= 0) {      /* traverse the transaction's items */
        i = *set++ -n;          /* compute counter vector index */
        if (i >= node->size) return;
        node->cnts[i]++;        /* if the counter exists, */
      } }                       /* count the transaction */
    else if (node->chcnt > 0) { /* if there are child nodes */
/*      vec = (ISNODE**)(node->cnts +node->size); */
      vec = get_vec(node);
      n   = ID(vec[0]);         /* get the child node vector */
      min--;                    /* one item less to the deepest nodes */
      while ((cnt > min) && (*set < n)) {
        cnt--; set++; }         /* skip items before first child */
      while (--cnt >= min) {    /* traverse the transaction's items */
        i = *set++ -n;          /* compute child vector index */
        if (i >= node->chcnt) return;
        if (vec[i]) _count(vec[i], set, cnt, min);
      }                         /* if the child exists, */
    } }                         /* count the transaction recursively */
  else {                        /* if an identifer map is used */
    map = node->cnts +(n = node->size);
    if (node->chcnt == 0) {     /* if this is a new node */
      while (--cnt >= 0) {      /* traverse the transaction's items */
        if (*set > map[n-1]) return;  /* if beyond last item, abort */
        i = _bsearch(map, n, *set++);
        if (i >= 0) node->cnts[i]++;
      } }                       /* find index and count transaction */
    else if (node->chcnt > 0) { /* if there are child nodes */
      vec = (ISNODE**)(map +n); /* get id. map and child vector */
      if (node->chcnt < n)      /* if a secondary id. map exists */
        map = (int*)(vec +(n = node->chcnt));
      min--;                    /* one item less to the deepest nodes */
      while (--cnt >= min) {    /* traverse the transaction's items */
        if (*set > map[n-1]) return;  /* if beyond last item, abort */
        i = _bsearch(map, n, *set++);
        if ((i >= 0) && vec[i]) _count(vec[i], set, cnt, min);
      }                         /* search for the proper index */
    }                           /* and if the child exists, */
  }                             /* count the transaction recursively */
}  /* _count() */

/*--------------------------------------------------------------------*/

static void _countx (ISNODE *node, TATREE *tat, int min)
{                               /* --- count t.a. tree recursively */
  int    i, k;                  /* vector index, loop variable */
  int    *map, n;               /* identifier map and its size */
  ISNODE **vec;                 /* child node vector */

  assert(node && tat);          /* check the function arguments */
  if (tat_max(tat) < min)       /* if the transactions are too short, */
    return;                     /* abort the recursion */
  k = tat_size(tat);            /* get the number of children */
  if (k <= 0) {                 /* if there are no children */
    if (k < 0) _count(node, tat_items(tat), -k, min);
    return;                     /* count the normal transaction */
  }                             /* and abort the function */
  while (--k >= 0)              /* count the transactions recursively */
    _countx(node, tat_child(tat, k), min);
  if (node->offset >= 0) {      /* if a pure vector is used */
    if (node->chcnt == 0) {     /* if this is a new node */
      n = node->offset;         /* get the index offset */
      for (k = tat_size(tat); --k >= 0; ) {
        i = tat_item(tat,k) -n; /* traverse the items */
        if (i < 0) return;      /* if before first item, abort */
        if (i < node->size)     /* if inside the counter range */
          node->cnts[i] += tat_cnt(tat_child(tat, k));
      } }                       /* count the transaction */
    else if (node->chcnt > 0) { /* if there are child nodes */
  /*    vec = (ISNODE**)(node->cnts +node->size); */
      vec = get_vec(node);
      n   = ID(vec[0]);         /* get the child node vector */
      min--;                    /* one item less to the deepest nodes */
      for (k = tat_size(tat); --k >= 0; ) {
        i = tat_item(tat,k) -n; /* traverse the items */
        if (i < 0) return;      /* if before first item, abort */
        if ((i < node->chcnt) && vec[i])
          _countx(vec[i], tat_child(tat, k), min);
      }                         /* if the child exists, */
    } }                         /* count the transaction recursively */
  else {                        /* if an identifer map is used */
    map = node->cnts +(n = node->size);
    if (node->chcnt == 0) {     /* if this is a new node */
      for (k = tat_size(tat); --k >= 0; ) {
        i = tat_item(tat, k);   /* get the next item */
        if (i < map[0]) return; /* if before first item, abort */
        i = _bsearch(map, n, i);
        if (i >= 0) node->cnts[i] += tat_cnt(tat_child(tat, k));
      } }                       /* find index and count transaction */
    else if (node->chcnt > 0) { /* if there are child nodes */
      vec = (ISNODE**)(map +n); /* get id. map and child vector */
      if (node->chcnt < n)      /* if a secondary id. map exists */
        map = (int*)(vec +(n = node->chcnt));
      min--;                    /* one item less to the deepest nodes */
      for (k = tat_size(tat); --k >= 0; ) {
        i = tat_item(tat, k);   /* get the next item */
        if (i < map[0]) return; /* if before first item, abort */
        i = _bsearch(map, n, i);
        if ((i >= 0) && vec[i]) _countx(vec[i], tat_child(tat, k), min);
      }                         /* search for the proper index */
    }                           /* and if the child exists, */
  }                             /* count the transaction recursively */
}  /* _countx() */

/*--------------------------------------------------------------------*/

static int _stskip (ISNODE *node)
{                               /* --- set subtree skip flags */
  int    i, r;                  /* vector index, result */
  ISNODE **vec;                 /* child node vector */

  assert(node);                 /* check the function argument */
  if (node->chcnt  == 0) return  0;  /* do not skip new leaves */
  if (node->chcnt  <  0) return -1;  /* skip marked subtrees */
  if (node->offset >= 0)        /* if a pure vector is used */
/*    vec = (ISNODE**)(node->cnts +node->size);*/
    vec = get_vec(node);
  else                          /* if an identifer map is used */
    vec = (ISNODE**)(node->cnts +node->size +node->size);
  for (r = -1, i = node->chcnt; --i >= 0; )
    if (vec[i]) r &= _stskip(vec[i]);
  if (!r) return 0;             /* recursively check all children */
  node->chcnt |= F_SKIP;        /* set the skip flag if possible */
  return -1;                    /* return 'skip subtree' */
}  /* _stskip() */

/*--------------------------------------------------------------------*/

static int _check (ISNODE *node, char *marks, int supp)
{                               /* --- recursively check item usage */
  int    i, r = 0;              /* vector index, result of check */
  int    *map, n;               /* identifier map and its size */
  ISNODE **vec;                 /* child node vector */

  assert(node && marks);        /* check the function arguments */
  if (node->offset >= 0) {      /* if a pure vector is used */
    if (node->chcnt == 0) {     /* if this is a new node */
      n = node->offset;         /* get the index offset */
      for (i = node->size; --i >= 0; ) {
        if (node->cnts[i] >= supp)
          marks[n+i] = r = 1;   /* mark items in set that satisfies */
      } }                       /* the minimum support criterion */
    else if (node->chcnt > 0) { /* if there are child nodes */
/*      vec = (ISNODE**)(node->cnts +node->size); */
      vec = get_vec(node);
      for (i = node->chcnt; --i >= 0; )
        if (vec[i]) r |= _check(vec[i], marks, supp);
    } }                         /* recursively process all children */
  else {                        /* if an identifer map is used */
    map = node->cnts +node->size;
    if (node->chcnt == 0) {     /* if this is a new node */
      for (i = node->size; --i >= 0; ) {
        if (node->cnts[i] >= supp)
          marks[map[i]] = r = 1;/* mark items in set that satisfies */
      } }                       /* the minimum support criterion */
    else if (node->chcnt > 0) { /* if there are child nodes */
      vec = (ISNODE**)(map +node->size);
      for (i = node->chcnt; --i >= 0; )
        if (vec[i]) r |= _check(vec[i], marks, supp);
    }                           /* get the child vector and */
  }                             /* recursively process all children */
  if ((r != 0) && node->parent) /* if the check succeeded, mark */
    marks[ID(node)] = 1;        /* the item associated with the node */
  return r;                     /* return the check result */
}  /* _check() */

/*--------------------------------------------------------------------*/

static int _getsupp (ISNODE *node, int *set, int cnt)
{                               /* --- get support of an item set */
  int    i, n, c;               /* vector index, buffers */
  int    *map;                  /* identifier map */
  ISNODE **vec;                 /* vector of child nodes */

  assert(node && set && (cnt >= 0)); /* check the function arguments */
  while (--cnt > 0) {           /* follow the set/path from the node */
    c = node->chcnt & ~F_SKIP;  /* if there are no children, */
    if (c <= 0) return -1;      /* the support is less than minsupp */
    if (node->offset >= 0) {    /* if a pure vector is used */
/*      vec = (ISNODE**)(node->cnts +node->size); */
      vec = get_vec(node);
      i   = *set++ -ID(vec[0]); /* compute the child vector index and */
      if (i >= c) return -1; }  /* abort if the child does not exist */
    else {                      /* if an identifier map is used */
      map = node->cnts +(n = node->size);
      vec = (ISNODE**)(map +n); /* get id. map and child vector */
      if (c < n)                /* if a secondary id. map exists, */
        map = (int*)(vec +(n = c));    /* get this identifier map */
      i = _bsearch(map, n, *set++);
    }                           /* search for the proper index */
    if (i < 0) return -1;       /* abort if index is out of range */
    node = vec[i];              /* go to the corresponding child */
    if (!node) return -1;       /* if the child does not exists, */
  }                             /* the support is less than minsupp */
  if (node->offset >= 0) {      /* if a pure vector is used, */
    i = *set -node->offset;     /* compute the counter index */
    if (i >= node->size) return -1; }
  else {                        /* if an identifier map is used */
    map = node->cnts +(n = node->size);
    i   = _bsearch(map, n, *set);
  }                             /* search for the proper index */
  if (i < 0) return -1;         /* abort if index is out of range */
  return node->cnts[i];         /* return the item set support */
}  /* _getsupp() */

/*--------------------------------------------------------------------*/

static void _clrsupp (ISNODE *node, int *set, int cnt, int supp)
{                               /* --- clear support of an item set */
  int    i, n, c;               /* vector index, buffers */
  int    *map;                  /* identifier map */
  ISNODE **vec;                 /* vector of child nodes */

  assert(node && set && (cnt >= 0)); /* check the function arguments */
  while (--cnt > 0) {           /* follow the set/path from the node */
    if (node->offset >= 0) {    /* if a pure vector is used */
      /* vec = (ISNODE**)(node->cnts +node->size); */
      vec = get_vec(node);
      i   = *set++ -ID(vec[0]);}/* compute the child vector index */
    else {                      /* if an identifier map is used */
      map = node->cnts +(n = node->size);
      vec = (ISNODE**)(map +n); /* get id. map, child vector and */
      c   = node->chcnt & ~F_SKIP;     /* the number of children */
      if (c < n)                /* if a secondary id. map exists, */
        map = (int*)(vec +(n = c));    /* get this identifier map */
      i = _bsearch(map, n, *set++);
    }                           /* search for the proper index */
    node = vec[i];              /* go to the corresponding child */
  }
  if (node->offset >= 0)        /* if a pure vector is used, */
    i = *set -node->offset;     /* compute the counter index */
  else {                        /* if an identifier map is used */
    map = node->cnts +(n = node->size);
    i   = _bsearch(map, n, *set);
  }                             /* search for the proper index */
  if ((supp < 0)                /* if to clear unconditionally */
  ||  (node->cnts[i] == supp))  /* or the support is the same */
    node->cnts[i] = -(node->cnts[i] & ~F_SKIP);
}  /* _clrsupp() */             /* clear the item set support */

/*--------------------------------------------------------------------*/

static ISNODE* _child (ISTREE *ist, ISNODE *node, int index,
                       int s_min, int s_sub)
{                               /* --- create child node (extend set) */
  int    i, k, n;               /* loop variables, counters */
  ISNODE *curr;                 /* to traverse the path to the root */
  int    item, cnt;             /* item identifier, number of items */
  int    *set;                  /* next (partial) item set to check */
  int    body;                  /* enough support for a rule body */
  int    hdonly;                /* whether head only item on path */
  int    app;                   /* appearance flags of an item */
  int    s_set;                 /* support of an item set */

  assert(ist && node            /* check the function arguments */
     && (index >= 0) && (index < node->size));
  if (node->offset >= 0) item = node->offset +index;
  else                   item = node->cnts[node->size +index];
  app = ist->apps[item];        /* get item id. and appearance flag */
  if ((app == IST_IGNORE)       /* do not extend an item to ignore */
  ||  ((HDONLY(node) && (app == IST_HEAD))))
    return NULL;                /* nor a set with two head only items */
  hdonly = HDONLY(node) || (app == IST_HEAD);

  /* --- initialize --- */
  s_set = node->cnts[index];    /* get support of item set to extend */
  if (s_set < s_min)            /* if set support is insufficient, */
    return NULL;                /* no child is needed, so abort */
  body = (s_set >= s_sub)       /* if the set has enough support for */
       ? 1 : 0;                 /* a rule body, set the body flag */
  ist->buf[ist->lvlvsz -2] = item;   /* init. set for support checks */

  /* --- check candidates --- */
  for (n = 0, i = index; ++i < node->size; ) {
    if (node->offset >= 0) k = node->offset +i;
    else                   k = node->cnts[node->size +i];
    app = ist->apps[k];         /* traverse the candidate items */
    if ((app == IST_IGNORE) || (hdonly && (app == IST_HEAD)))
      continue;                 /* skip sets with two head only items */
    s_set = node->cnts[i];      /* traverse the candidate items */
    if (s_set <  s_min)         /* if set support is insufficient, */
      continue;                 /* ignore the corresponding candidate */
    body &= 1;                  /* restrict body flags to the set S */
    if (s_set >= s_sub)         /* if set support is sufficient for */
      body |= 2;                /* a rule body, set the body flag */ 
    set    = ist->buf +ist->lvlvsz -(cnt = 2);
    set[1] = k;                 /* add the candidate item to the set */
    for (curr = node; curr->parent; curr = curr->parent) {
      s_set = _getsupp(curr->parent, set, cnt);
      if (s_set <  s_min)      /* get the item set support and */
        break;                  /* if it is too low, abort the loop */
      if (s_set >= s_sub)       /* if some subset has enough support */
        body |= 4;              /* for a rule body, set the body flag */
      *--set = ID(curr); cnt++; /* add id of current node to the set */
    }                           /* and adapt the number of items */
    if (!curr->parent && body)  /* if subset support is high enough */
      ist->map[n++] = k;        /* for a full rule and a rule body, */
  }                             /* note the item identifier */
  if (n <= 0) return NULL;      /* if no child is needed, abort */
#ifdef BENCH                  /* if benchmark version: */
  ist->nec += n;                /* sum the necessary counters */
#endif

  /* --- decide on node structure --- */
  k = ist->map[n-1] -ist->map[0] +1;
  if    (!ist->memopt) n = k;   /* check the size of a pure vector */
  else if (3*n >= 2*k) n = k;   /* use a pure vector if it is small */
  else                 k = n+n; /* enough, otherwise use an id. map */
#ifdef BENCH                  /* if benchmark version */
  ist->cnt   += n;              /* sum the number of counters */
  ist->bytes += sizeof(ISNODE) +(k-1) *sizeof(int) +8;
#endif                        /* determine the memory usage */

  /* --- create child --- */
#ifdef ARCH64               /* if 64 bit architecture */
  curr = (ISNODE*)malloc(sizeof(ISNODE) + ((k&1)?k:(k-1)) *sizeof(int));
#else
  curr = (ISNODE*)malloc(sizeof(ISNODE) +(k-1) *sizeof(int));
#endif                        /* pad to even number of counters */
  if (!curr) return (void*)-1;  /* create a child node */
  curr->parent = node;          /* set pointer to parent node */
  curr->succ   = NULL;          /* and clear successor pointer */
  curr->id     = item;          /* initialize the item id. and */
  if (hdonly) curr->id |= F_HDONLY;  /* set the head only flag */
  curr->chcnt  = 0;             /* there are no children yet */
  curr->size   = n;             /* set size of counter vector */
  if (n == k)                   /* if to use a pure vector, */
    curr->offset = ist->map[0]; /* note the first item as an offset */
  else {                        /* if to use an identifier map, */
    curr->offset = -1;          /* use the offset as an indicator */
    for (set = curr->cnts +n +(i = n); --i >= 0; )
      *--set = ist->map[i];     /* copy the identifier map */
  }                             /* from the buffer to the node */
  for (set = curr->cnts +(i = n); --i >= 0; )
    *--set = 0;                 /* clear all counters of the node */
  return curr;                  /* return pointer to created child */
}  /* _child() */

/*----------------------------------------------------------------------
  In the above function the set S represented by the index-th vector
element of the current node is extended only by combining it with the
sets represented by the fields that follow it in the node vector,
i.e. by the sets represented by vec[index+1] to vec[size-1]. The sets
that can be formed by combining the set S and the sets represented by
vec[0] to vec[index-1] are processed in the branches for these sets.
  In the 'check candidates' loop it is checked for each set represented
by vec[index+1] to vec[size-1] whether this set and all other subsets
of the same size, which can be formed from the union of this set and
the set S, have enough support, so that a child node is necessary.
  Note that i +offset is the identifier of the item that has to be
added to set S to form the union of the set S and the set T represented
by vec[i], since S and T have the same path with the exception of the
index in the current node. Hence we can speak of candidate items that
are added to S.
  Checking the support of the other subsets of the union of S and T
that have the same size as S and T is done with the aid of a path
variable. The items in this variable combined with the items on the
path to the current node always represent the subset currently tested.
That is, the path variable holds the path to be followed from the
current node to arrive at the support counter for the subset. The path
variable is initialized to [0]: <item>, [1]: <offset+i>, since the
support counters for S and T can be inspected directly. Then this
path is followed from the parent node of the current node, which is
equivalent to checking the subset that can be obtained by removing
from the union of S and T the item that corresponds to the parent node
(in the path to S or T, resp.).
  Iteratively making the parent node the current node, adding its
corresponding item to the path and checking the support counter at the
end of the path variable when starting from its (the new current node's)
parent node tests all other subsets.
  Another criterion is that the extended set must not contain two items
which may appear only in the head of a rule. If two such items are
contained in a set, neither can a rule be formed from its items nor can
it be the antecedent of a rule. Whether a set contains two head only
items is determined from the nodes 'hdonly' flag and the appearance
flags of the items.
----------------------------------------------------------------------*/

static void _cleanup (ISTREE *ist)
{                               /* --- clean up on error */
  ISNODE *node, *t;             /* to traverse the nodes */

  assert(ist);                  /* check the function argument */
  for (node = ist->levels[ist->lvlcnt]; node; ) {
    t = node; node = node->succ; free(t); }
  ist->levels[ist->lvlcnt] = NULL; /* delete all created nodes */
  for (node = ist->levels[ist->lvlcnt -1]; node; node = node->succ)
    node->chcnt = 0;            /* clear the child node counters */
}  /* _cleanup() */             /* of the deepest nodes in the tree */

/*----------------------------------------------------------------------
  Additional Evaluation Measure Functions
----------------------------------------------------------------------*/

static double _none (double head, double body, double post)
{ return 1; }                   /* --- no add. evaluation measure */

/*--------------------------------------------------------------------*/

static double _diff (double head, double body, double post)
{ return fabs(post -head); }    /* --- absolute confidence difference */

/*--------------------------------------------------------------------*/

static double _quot (double head, double body, double post)
{                               /* --- diff. of conf. quotient to 1 */
  if (post > head) return 1 -head/post;
  return (head <= 0) ? 0 : (1 -post/head);
}  /* _quot() */

/*--------------------------------------------------------------------*/

static double _aimp (double head, double body, double post)
{                               /* --- abs. diff. of improvement to 1 */
  return (head <= 0) ? 0 : fabs(1 -post/head);
}  /* _aimp() */

/*--------------------------------------------------------------------*/

static double _info (double head, double body, double post)
{                               /* --- information diff. to prior */
  double res, t;                /* result, temporary buffer */

  if ((head < EPSILON) || (1-head < EPSILON)
  ||  (body < EPSILON) || (1-body < EPSILON))
    return 0;                   /* check for strict positivity */
  post *= body; res = 0;        /* support of     head and     body */
  if (post > 0) res += post *log(post /(   head  *   body));
  t = body -post;               /* support of not head and     body */
  if (t    > 0) res += t    *log(t    /((1-head) *   body));
  t = head -post;               /* support of     head and not body */
  if (t    > 0) res += t    *log(t    /(   head  *(1-body)));
  t = 1-head -body +post;       /* support of not head and not body */
  if (t    > 0) res += t    *log(t    /((1-head) *(1-body)));
  return res/LN_2;              /* return information gain in bits */
}  /* _info() */

/*--------------------------------------------------------------------*/

static double _chi2 (double head, double body, double post)
{                               /* --- normalized chi^2 measure */
  double t;                     /* temporary buffer */

  if ((head < EPSILON) || (1-head < EPSILON)
  ||  (body < EPSILON) || (1-body < EPSILON))
    return 0;                   /* check for strict positivity */
  t = (head -post) *body;       /* compute and return chi^2 measure */
  return (t*t) / (head *(1-head) *body *(1-body));
}  /* _chi2() */

/*--------------------------------------------------------------------*/

static EVALFN *_evalfns[EM_UNKNOWN] = {
  /* EM_NONE  0 */  _none,      /* no additional evaluation measure */
  /* EM_DIFF  1 */  _diff,      /* absolute confidence difference */
  /* EM_QUOT  2 */  _quot,      /* difference of conf. quotient to 1 */
  /* EM_AIMP  3 */  _aimp,      /* abs. diff. of improvement to 1 */
  /* EM_INFO  4 */  _info,      /* information difference to prior */
  /* EM_CHI2  5 */  _chi2,      /* normalized chi^2 measure */
};                              /* table of evaluation functions */

/*----------------------------------------------------------------------
  Main Functions
----------------------------------------------------------------------*/

ISTREE* ist_create (int itemcnt, double supp, double conf,
                    int rsdef, const char *apps, int memopt)
{                               /* --- create an item set tree */
  ISTREE *ist;                  /* created item set tree */
  ISNODE **lvl;                 /* vector of tree levels */
  ISNODE *root;                 /* root node of the tree */
  int    *buf, *map;            /* buffer vector, identifier map */
  char   *a;                    /* to traverse appearances vector */
  int    n;                     /* temporary buffer */

  assert((itemcnt >= 0)         /* check the function arguments */
      && (supp >= 0) && (supp <= 1) && (conf >= 0) && (conf <= 1));

  n = itemcnt;                  /* on 32 bit systems, however, */

  /* --- allocate memory --- */ 
#ifdef ARCH64                 /* if 64 bit architecture */
  ist = (ISTREE*)malloc(sizeof(ISTREE) +((n&1)?n:(n-1)) *sizeof(char));
#else                         /* pad counters to even number */
  ist = (ISTREE*)malloc(sizeof(ISTREE) +(n-1) *sizeof(char));
#endif                        /* use the number of items directly */
  if (!ist) return NULL;        /* allocate the tree body */
  ist->levels = lvl = (ISNODE**)malloc(BLKSIZE *sizeof(ISNODE*));
  if (!lvl) { free(ist); return NULL; }
  ist->buf    = buf = (int*)    malloc(BLKSIZE *sizeof(int));
  if (!buf) { free(lvl); free(ist); return NULL; }
  ist->map    = map = (int*)    malloc(itemcnt *sizeof(int));
  if (!map) { free(buf); free(lvl); free(ist); return NULL; }

  lvl[0] = ist->curr = root =   /* allocate a root node */
    (ISNODE*)calloc(1, sizeof(ISNODE) +(n-1) *sizeof(int));
  if (!root){ free(map); free(buf); free(lvl); free(ist); return NULL; }

  /* --- initialize structures --- */
  ist->lvlvsz  = BLKSIZE;       /* copy parameters to the structure */
  ist->lvlcnt  = 1;    ist->tacnt = 0;
  ist->supp    = supp; ist->conf  = conf;
  ist->rsdef   = rsdef & IST_BOTH;
  ist->memopt  = memopt;
#ifdef BENCH                  /* if benchmark version */
  ist->cnt     = ist->nec   = itemcnt;
  ist->chcnt   = ist->chnec = 0;
  ist->bytes   = sizeof(ISTREE) +itemcnt *sizeof(char) +8
               + BLKSIZE *sizeof(ISNODE*) +8
               + BLKSIZE *sizeof(int) +8
               + itemcnt *sizeof(int) +8;
#endif                        /* initialize the benchmark variables */
  ist_init(ist, 1, EM_NONE, 1); /* initialize rule extraction */
  root->parent = root->succ = NULL;
  root->offset = root->id   = 0;
  root->chcnt  = 0;             /* initialize the root node */
  root->size   = n;
  a = ist->apps;                /* copy item appearances */
  if (apps) { while (--itemcnt >= 0) *a++ = *apps++ & IST_BOTH; }
  else      { while (--itemcnt >= 0) *a++ = IST_BOTH;           }
  return ist;                   /* return created item set tree */
}  /* ist_create() */

/*--------------------------------------------------------------------*/

void ist_delete (ISTREE *ist)
{                               /* --- delete an item set tree */
  int    i;                     /* loop variables */
  ISNODE *node, *t;             /* to traverse the nodes */

  assert(ist);                  /* check the function argument */
  for (i = ist->lvlcnt; --i >= 0; ) {
    for (node = ist->levels[i]; node; ) {
      t = node; node = node->succ; free(t); }
  }                             /* delete all nodes, */
  free(ist->levels);            /* the level vector, */
  free(ist->map);               /* the identifier map, */
  free(ist->buf);               /* the path buffer, */
  free(ist);                    /* and the tree body */
}  /* ist_delete() */

/*--------------------------------------------------------------------*/

void ist_count (ISTREE *ist, int *set, int cnt)
{                               /* --- count transaction in tree */
  assert(ist                    /* check the function arguments */
     && (cnt >= 0) && (set || (cnt <= 0)));
  if (cnt >= ist->lvlcnt)       /* recursively count transaction */
    _count(ist->levels[0], set, cnt, ist->lvlcnt);
  ist->tacnt++;                 /* increment the transaction counter */
}  /* ist_count() */

/*--------------------------------------------------------------------*/

void ist_countx (ISTREE *ist, TATREE *tat)
{                               /* --- count transaction in tree */
  assert(ist && tat);           /* check the function arguments */
  _countx(ist->levels[0], tat, ist->lvlcnt);
  ist->tacnt = tat_cnt(tat);    /* recursively count the tree and */
}  /* ist_countx() */           /* set the transaction counter */

/*--------------------------------------------------------------------*/

int ist_check (ISTREE *ist, char *marks)
{                               /* --- check item usage */
  int i, n;                     /* loop variable, number of items */

  assert(ist);                  /* check the function argument */
  for (i = ist->levels[0]->size; --i >= 0; )
    marks[i] = 0;               /* clear the marker vector */
  n = (ist->rsdef == IST_BOTH)  /* get the minimum support */
    ? (int)ceil(ist->tacnt *ist->supp)
    : (int)ceil(ist->tacnt *ist->supp *ist->conf);
  _check(ist->levels[0], marks, n);  /* check the item usage */
  for (n = 0, i = ist->levels[0]->size; --i >= 0; )
    if (marks[i]) n++;          /* count used items */
  return n;                     /* and return this number */
}  /* ist_check() */

/*--------------------------------------------------------------------*/

int ist_addlvl (ISTREE *ist)
{                               /* --- add a level to item set tree */
  int    i, n, c;               /* loop variable, counter, buffer */
  ISNODE **ndp;                 /* to traverse the nodes */
  ISNODE *node;                 /* new (reallocated) node */
  ISNODE **end;                 /* end of new level node list */
  ISNODE *cur;                  /* current node in new level */
  ISNODE *frst;                 /* first child of current node */
  ISNODE *last;                 /* last  child of current node */
  ISNODE **vec;                 /* child node vector */
  int    *map;                  /* identifier map */
  int    s_min;                 /* minimal support of a set */
  int    s_sub;                 /* minimal support of a subset */
  void   *p;                    /* temporary buffer */

  assert(ist);                  /* check the function arguments */

  /* --- enlarge level vector --- */
  if (ist->lvlcnt >= ist->lvlvsz) { /* if the level vector is full */
    n = ist->lvlvsz +BLKSIZE;   /* compute new vector size */
    p = realloc(ist->levels, n *sizeof(ISNODE*));
    if (!p) return -1;          /* enlarge the level vector */
    ist->levels = (ISNODE**)p;  /* and set the new vector */
    p = realloc(ist->buf,    n *sizeof(int));
    if (!p) return -1;          /* enlarge the buffer vector */
    ist->buf    = (int*)p;      /* and set the new vector */
    ist->lvlvsz = n;            /* set the new vector size */
  }                             /* (applies to buf and levels) */
  end  = ist->levels +ist->lvlcnt;
  *end = NULL;                  /* start a new tree level */

  /* --- add tree level --- */
  s_sub = (int)ceil(ist->tacnt *ist->supp); /* minimal subset support */
  s_min = (ist->rsdef == IST_BOTH) ? s_sub  /* minimal rule   support */
        : (int)ceil(ist->tacnt *ist->supp *ist->conf);
  for (ndp = ist->levels +ist->lvlcnt -1; *ndp; ndp = &(*ndp)->succ) {
    frst = last = NULL;         /* traverse the deepest nodes */
    for (i = n = 0; i < (*ndp)->size; i++) {
      cur = _child(ist, *ndp, i, s_min, s_sub);
      if (!cur) continue;       /* create a child if necessary */
      if (cur == (void*)-1) { _cleanup(ist); return -1; }
      if (!frst) frst = cur;    /* note first and last child node */
      *end = last = cur;        /* add node at the end of the list */
      end  = &cur->succ; n++;   /* that contains the new level */
    }                           /* and advance end pointer */
    if (n <= 0) {               /* if no child node was created, */
      (*ndp)->chcnt = F_SKIP; continue; }       /* skip the node */
#ifdef BENCH                /* if benchmark version */
    ist->chnec += n;            /* sum the number of necessary */
#endif                      /* child pointers */
    node = *ndp;                /* decide on the node structure: */
    if (node->offset >= 0) {    /* if a pure counter vector is used, */
      n = ID(last)-ID(frst)+1;  /* always add a pure child vector */
      i = (node->size -1) *sizeof(int) +n *sizeof(ISNODE*); }
    else if (2*n > node->size){ /* if a single id. map is best, */
      n = node->size;           /* only add a child vector */
      i = (n+n-1) *sizeof(int) +n *sizeof(ISNODE*); }
    else {                      /* if two identifier maps are best, */
      i = node->size;           /* add a child vector and a map */
      i = (i+i-1) *sizeof(int) +n *(sizeof(ISNODE*) +sizeof(int));
    }                           /* get size of additional vectors */
    node = (ISNODE*)realloc(node, sizeof(ISNODE) +i);
    if (!node) { _cleanup(ist); return -1; }
    node->chcnt = n;            /* add a child vector to the node */
#ifdef BENCH                /* if benchmark version */
    ist->chcnt += n;            /* sum the number of child pointers */
    if ((node->offset >= 0) || (node->size == n))
         ist->bytes += n * sizeof(ISNODE*);
    else ist->bytes += n *(sizeof(ISNODE*) +sizeof(int));
#endif                      /* determine the memory usage */
    if ((node != *ndp) && node->parent) {
      last = node->parent;      /* adapt the ref. from the parent */
      if (last->offset >= 0) {  /* if a pure vector is used */
/*        vec = (ISNODE**)(last->cnts +last->size); */
        vec = get_vec(last);
        vec[(vec[0] != *ndp) ? ID(node) -ID(vec[0]) : 0] = node; }
      else {                    /* if an identifier map is used */
        map = last->cnts +(i = last->size);
        vec = (ISNODE**)(map+i);/* get identifier map, child vector, */
        c   = last->chcnt & ~F_SKIP;   /* and the number of children */
        if (c < i)              /* if a secondary id. map exists, */
          map = (int*)(vec +(i = c));  /* get this identifier map */
        vec[_bsearch(map, i, node->id)] = node;
      }                         /* find the proper index and */
    }                           /* set the new child pointer */
    *ndp = node;                /* set new (reallocated) node */
    if (node->offset >= 0) {    /* if to use pure vectors */
/*      vec = (ISNODE**)(node->cnts +node->size); */
      vec = get_vec(node);
      while (--n >= 0) vec[n] = NULL;
      i = ID(frst);             /* get item identifier of first child */
      for (cur = frst; cur; cur = cur->succ) {
        vec[ID(cur)-i] = cur;   /* set the child node pointer */
        cur->parent    = node;  /* and the parent pointer */
      } }                       /* in the new node */
    else if (n < node->size) {  /* if two identifier maps are used */
      vec = (ISNODE**)(node->cnts +node->size +node->size);
      map = (int*)(vec +n);     /* get the secondary identifier map */
      for (i = 0, cur = frst; cur; cur = cur->succ) {
        vec[i]      = cur;      /* set the child node pointer, */
        map[i++]    = cur->id;  /* the identifier map entry, */
        cur->parent = node;     /* and the parent pointer */
      } }                       /* in the new node */
    else {                      /* if one identifier map is used */
      map = node->cnts +(i = node->size);
      vec = (ISNODE**)(map +i); /* get id. map and child vector */
      while (--n >= 0) vec[n] = NULL;
      for (cur = frst; cur; cur = cur->succ) {
        vec[_bsearch(map, i, cur->id)] = cur;
        cur->parent = node;     /* set the child node pointer */
      }                         /* and the parent pointer */
    }                           /* in the new node */
  }
  if (!ist->levels[ist->lvlcnt])/* if no child has been added, */
    return 1;                   /* abort the function, otherwise */
  ist->lvlcnt++;                /* increment the level counter */
  ist->tacnt = 0;               /* clear the transaction counter and */
  ist->node  = NULL;            /* the item set node for rule extr. */
  _stskip(ist->levels[0]);      /* mark subtrees to be skipped */
  return 0;                     /* return 'ok' */
}  /* ist_addlvl() */

/*--------------------------------------------------------------------*/

void ist_up (ISTREE *ist, int root)
{                               /* --- go up in item set tree */
  assert(ist && ist->curr);     /* check the function argument */
  if      (root)                /* if root flag set, */
    ist->curr = ist->levels[0]; /* go to the root node */
  else if (ist->curr->parent)   /* if it exists, go to the parent */
    ist->curr = ist->curr->parent;
}  /* ist_up() */

/*--------------------------------------------------------------------*/

int ist_down (ISTREE *ist, int item)
{                               /* --- go down in item set tree */
  ISNODE *node;                 /* the current node */
  ISNODE **vec;                 /* child node vector of current node */
  int    *map, n;               /* identifier map and its size */
  int    c;                     /* number of children */

  assert(ist && ist->curr);     /* check the function argument */
  node = ist->curr;             /* get the current node */
  c = node->chcnt & ~F_SKIP;    /* if there are no child nodes, */
  if (c <= 0) return -1;        /* abort the function */
  if (node->offset >= 0) {      /* if a pure vector is used */
/*    vec = (ISNODE**)(node->cnts +node->size); */
    vec = get_vec(node);
    item -= ID(vec[0]);         /* compute index in child node vector */
    if (item >= c) return -1; } /* and abort if there is no child */
  else {                        /* if an identifier map is used */
    map = node->cnts +(n = node->size);
    vec = (ISNODE**)(map +n);   /* get id. map and child vector */
    if (c < n)                  /* if a secondary id. map exists, */
      map = (int*)(vec +(n = c));      /* get this identifier map */
    item = _bsearch(map, n, item);
  }                             /* search for the proper index */
  if ((item < 0) || !vec[item]) /* if the index is out of range */
    return -1;                  /* or the child does not exist, abort */
  ist->curr = vec[item];        /* otherwise go to the child node */
  return 0;                     /* return 'ok' */
}  /* ist_down() */

/*--------------------------------------------------------------------*/

int ist_next (ISTREE *ist, int item)
{                               /* --- get next item with a counter */
  int    i;                     /* vector index */
  ISNODE *node;                 /* the current node */
  int    *map, n;               /* identifier map and its size */

  assert(ist && ist->curr);     /* check the function argument */
  node = ist->curr;             /* get the current node */
  if (node->offset >= 0) {      /* if a pure vector is used, */
    if (item <  node->offset) return node->offset;
    if (item >= node->offset +node->size) return -1;
    return item +1; }           /* return the next item identifier */
  else {                        /* if an identifier map is used */
    map = node->cnts +(n = node->size);
    if (item <  map[0])   return map[0];
    if (item >= map[n-1]) return -1;
    i = _bsearch(map, n, item); /* try to find the item directly */
    if (i >= 0) return map[i+1];/* and return the following one */
    while ((--n >= 0) && (*map > item)) map++;
    return (n >= 0) ? *map :-1; /* search iteratively for the next */
  }                             /* item identifier and return it */
}  /* ist_next() */

/*--------------------------------------------------------------------*/

void ist_setcnt (ISTREE *ist, int item, int cnt)
{                               /* --- set counter for an item */
  ISNODE *node;                 /* the current node */
  ISNODE **vec;                 /* child node vector of current node */
  int    *map, n;               /* identifier map and its size */
  int    c;                     /* number of children */

  assert(ist && ist->curr);     /* check the function argument */
  node = ist->curr;             /* get the current node */
  if (node->offset >= 0) {      /* if a pure vector is used, */
    item -= node->offset;       /* get index in counter vector */
    if (item >= node->size) return; }
  else {                        /* if an identifier map is used */
    map = node->cnts +(n = node->size);
    vec = (ISNODE**)(map +n);   /* get id. map and child vector */
    c = node->chcnt & ~F_SKIP;  /* and the number of children */
    if (c < n)                  /* if a secondary id. map exists, */
      map = (int*)(vec +(n = c));      /* get this identifier map */
    item = _bsearch(map, n, item);
  }                             /* search for the proper index */
  if (item >= 0) node->cnts[item] = cnt;
}  /* ist_setcnt() */           /* set the frequency counter */

/*--------------------------------------------------------------------*/

int ist_getcnt (ISTREE *ist, int item)
{                               /* --- get counter for an item */
  ISNODE *node;                 /* the current node */
  ISNODE **vec;                 /* child node vector of current node */
  int    *map, n;               /* identifier map and its size */
  int    c;                     /* number of children */

  assert(ist && ist->curr);     /* check the function argument */
  node = ist->curr;             /* get the current node */
  if (node->offset >= 0) {      /* if pure vectors are used, */
    item -= node->offset;       /* get index in counter vector */
    if (item >= node->size) return -1; }
  else {                        /* if an identifier map is used */
    map = node->cnts +(n = node->size);
    vec = (ISNODE**)(map +n);   /* get id. map and child vector */
    c = node->chcnt & ~F_SKIP;  /* and the number of children */
    if (c < n)                  /* if a secondary id. map exists, */
      map = (int*)(vec +(n = c));      /* get this identifier map */
    item = _bsearch(map, n, item);
  }                             /* search for the proper index */
  if (item < 0) return -1;      /* abort if index is out of range */
  return node->cnts[item];      /* return the value of the counter */
}  /* ist_getcnt() */

/*--------------------------------------------------------------------*/

int ist_getcntx (ISTREE *ist, int *set, int cnt)
{                               /* --- get counter for an item set */
  assert(ist                    /* check the function arguments */
     && (cnt >= 0) && (set || (cnt <= 0)));
  if (cnt <= 0)                 /* if the item set is empty, */
    return ist->tacnt;          /* return the transaction count */
  return _getsupp(ist->levels[0], set, cnt);
}  /* ist_getcntx() */          /* return the item set support */

/*--------------------------------------------------------------------*/

void ist_filter (ISTREE *ist, int mode)
{                               /* --- filter max. freq. item sets */
  int    i, k, n;               /* loop variables */
  ISNODE *node, *curr;          /* to traverse the nodes */
  int    s_min, supp;           /* (minimum) support of an item set */
  int    *set;                  /* next (partial) item set to process */

  assert(ist);                  /* check the function argument */
  s_min = (int)ceil(ist->tacnt *ist->supp);
  if (s_min <= 0) s_min = 1;    /* get minimal support */
  if (mode == IST_GENERATOR)
    for (n = ist->lvlcnt-1; n > 0; n--) {
      for (node = ist->levels[n]; node; node = node->succ) {
        for (i = 0; i < node->size; i++) {
          if (node->cnts[i] < s_min)
            continue;             /* skip infrequent item sets */
        
          if (node->offset >= 0) k = node->offset +i;
          else                   k = node->cnts[node->size +i];
          set    = ist->buf +ist->lvlvsz;
          *--set = k;       
          if( _getsupp(node->parent, set, 1)==node->cnts[i])
            node->cnts[i]=-(node->cnts[i] & ~F_SKIP);
          *--set = ID(node); 
          if( _getsupp(node->parent, set, 1)==node->cnts[i])
            node->cnts[i]=-(node->cnts[i] & ~F_SKIP);
          k = 2;                  /* clear counters in parent node */
          for (curr = node->parent; curr->parent; curr = curr->parent) {
            if(_getsupp(curr->parent, set, k)==node->cnts[i]){
              node->cnts[i]=-(node->cnts[i] & ~F_SKIP);
              break;
            }
            *--set = ID(curr); k++;
          }
        }
      }
    }
    else
      for (n = 1; n < ist->lvlcnt; n++) {
        for (node = ist->levels[n]; node; node = node->succ) {
          for (i = 0; i < node->size; i++) {
                if (node->cnts[i] < s_min)
                  continue;             /* skip infrequent item sets */

          supp = (mode == IST_CLOSED) ? node->cnts[i] : -1;
          if (node->offset >= 0) k = node->offset +i;
          else                   k = node->cnts[node->size +i];
          set    = ist->buf +ist->lvlvsz;
          *--set = k;        _clrsupp(node->parent, set, 1, supp);
          *--set = ID(node); _clrsupp(node->parent, set, 1, supp);
          k = 2;                  /* clear counters in parent node */
          for (curr = node->parent; curr->parent; curr = curr->parent) {
            _clrsupp(curr->parent, set, k, supp);
            *--set = ID(curr); k++;
        }/* climb up the tree and use the */
      }                         /* constructed (partial) item sets */
    }                           /* as paths to find the counters */
              /* that have to be cleared */
   }  /* ist_filter() */
}
/*--------------------------------------------------------------------*/

void ist_init (ISTREE *ist, int minlen, int arem, double minval)
{                               /* --- initialize (rule) extraction */
  assert(ist                    /* check the function arguments */
      && (minlen > 0) && (minval >= 0.0) && (minval <= 1.0));
  ist->index = ist->item = -1;
  ist->node  = ist->head = NULL;
  ist->size  = minlen;          /* initialize rule extraction */
  if ((arem < EM_NONE) || (arem >= EM_UNKNOWN))
    arem = EM_NONE;             /* check, adapt, and note */
  ist->arem   = arem;           /* additional evaluation measure */
  ist->minval = minval;         /* and its minimal value */
}  /* ist_init() */

/*--------------------------------------------------------------------*/

int ist_set (ISTREE *ist, int *set, double *supp, double *aval)
{                               /* --- extract next frequent item set */
  int    i;                     /* loop variable */
  int    item;                  /* an item identifier */
  ISNODE *node, *tmp;           /* current item set node, buffer */
  int    *cnts;                 /* to access the item frequencies */
  int    s_min;                 /* minimal support of a set */
  int    s_set;                 /* support of the current set */
  double ind;                   /* freq. of independent occurrence */
  double nrm;                   /* freq. normalization factor */

  assert(ist && set && supp);   /* check the function arguments */

  /* --- initialize --- */
  if (ist->size > ist->lvlcnt)  /* if the tree is not high enough */
    return -1;                  /* for the item set size, abort */
  s_min = (int)ceil(ist->tacnt *ist->supp); /* get minimal support */
  if (!ist->node)               /* on first item set, initialize */
    ist->node = ist->levels[ist->size-1];    /* the current node */
  node = ist->node;             /* get the current item set node */
  nrm  = (ist->tacnt > 0)       /* compute the normalization factor */
       ? 1.0/ist->tacnt : 1.0;  /* for the item set support and */
  cnts = ist->levels[0]->cnts;  /* get the item frequency vector */

  /* --- find frequent item set --- */
  while (1) {                   /* search for a frequent item set */
    if (++ist->index >= node->size) { /* if all subsets have been */
      node = node->succ;        /* processed, go to the successor */
      if (!node) {              /* if at the end of a level, go down */
        if (++ist->size > ist->lvlcnt)
          return -1;            /* if beyond the deepest level, abort */
        node = ist->levels[ist->size -1];
      }                         /* get the 1st node of the new level */
      ist->node  = node;        /* note the new item set node */
      ist->index = 0;           /* start with the first item set */
    }                           /* of the new item set node */
    if (node->offset >= 0) item = node->offset +ist->index;
    else                   item = node->cnts[node->size +ist->index];
    if (ist->apps[item] == IST_IGNORE)
      continue;                 /* skip items to ignore */
    s_set = node->cnts[ist->index];
    if (s_set < s_min)          /* if the support is not sufficient, */
      continue;                 /* go to the next item set */
    if (ist->size <  2) {       /* if this is a one element item set, */
      ind = 0; break; }         /* abort the search loop */
    if      (ist->arem == EM_LOGQ) { /* if measure is the logarithm */
      ind = log(abs(cnts[item]));    /* of the support quotient */
      for (tmp = node; tmp->parent; tmp = tmp->parent)
        ind += log(abs(cnts[ID(node)]) *nrm);
      ind = (log(s_set) -ind)   /* compute the binary logarithm */
          / (100*LN_2); }       /* of the support quotient */
    else if (ist->arem == EM_QUOT) { /* if measure is the quotient */
      ind = abs(cnts[item]);    /* compute the expected support */
      for (tmp = node; tmp->parent; tmp = tmp->parent)
        ind *= abs(cnts[ID(node)]) *nrm;
      ind = s_set /ind -1.0;  } /* compute the support quotient -1 */
    else {                      /* if no add. evaluation measure, */
      ind = 0; break; }         /* abort the search loop */
    if (ind >= ist->minval)     /* if the value of the additional */
      break;                    /* evaluation measure is high enough, */
  }                             /* abort the search loop */
  *supp = s_set *nrm;           /* compute the item set support */

  /* --- build frequent item set --- */
  i        = ist->size;         /* get the current item set size */
  set[--i] = item;              /* and store the first item */
  while (node->parent) {        /* while not at the root node */
    set[--i] = ID(node);        /* add item to the item set */
    node = node->parent;        /* and go to the parent node */
  }
  if (aval) *aval = ind;        /* set the add. evaluation measure */
  return ist->size;             /* return the item set size */
}  /* ist_set() */

/*--------------------------------------------------------------------*/

int ist_rule (ISTREE *ist, int *rule,
              double *supp, double *conf, double *lift, double *aval)
{                               /* --- extract next rule */
  int    i;                     /* loop variable */
  int    item;                  /* an item identifier */
  ISNODE *node;                 /* current item set node */
  ISNODE *parent;               /* parent of the item set node */
  /* ISNODE **vec;  (unused) */               /* child node vector */
  int    *map, n;               /* identifier map and its size */
  int    s_rule;                /* minimal support of a rule */
  int    s_min;                 /* minimal support of a set */
  int    s_set;                 /* support of set    (body & head) */
  int    s_sub;                 /* support of subset (body) */
  double p_body, p_head;        /* prior confidences/probabilities */
  double c, v;                  /* confidence and measure value */
  int    app;                   /* appearance flag of head item */

  assert(ist && rule && supp && conf);  /* check function arguments */

  /* --- initialize --- */
  if (ist->size > ist->lvlcnt)  /* if the tree is not high enough */
    return -1;                  /* for the rule length, abort */
  s_rule = (int)ceil(ist->tacnt *ist->supp);  /* minimal rule support */
  s_min  = (ist->rsdef == IST_BOTH) ? s_rule  /* minimal set  support */
         : (int)ceil(ist->tacnt *ist->supp *ist->conf);
  if (ist->node)                /* if this is not the first rule, */
    node = ist->node;           /* get the buffered item set node */
  else {                        /* if this is the first rule */
    node = ist->node = ist->levels[ist->size -1];
    ist->index = ist->item = -1;/* initialize the */
  }                             /* rule extraction variables */

  /* --- find rule --- */
  while (1) {                   /* search for a rule */
    if (ist->item >= 0) {       /* --- select next item subset */
      *--ist->path = ist->item; /* add previous head to the path */
      ist->plen++;              /* and get the next head item */
      ist->item = ID(ist->head);
      ist->head = ist->head->parent;
      if (!ist->head)           /* if all subsets have been processed */
        ist->item = -1;         /* clear the head item to trigger the */
    }                           /* selection of a new item set */
    if (ist->item < 0) {        /* --- select next item set */
      if (++ist->index >= node->size){/* if all subsets have been */
        node = node->succ;      /* processed, go to the successor */
        if (!node) {            /* if at the end of a level, go down */
          if (++ist->size > ist->lvlcnt)
            return -1;          /* if beyond the deepest level, abort */
          node = ist->levels[ist->size -1];
        }                       /* get the 1st node of the new level */
        ist->node = node;       /* note the new item set node and */
        ist->index  = 0;        /* start with the first item set */
      }                         /* of the new item set node */
      if (node->offset >= 0) item = node->offset +ist->index;
      else                   item = node->cnts[node->size +ist->index];
      if ((ist->apps[item] == IST_IGNORE)
      ||  (HDONLY(node) && (ist->apps[item] == IST_HEAD)))
        continue;               /* skip sets with two head only items */
      ist->item   = item;       /* set the head item identifier */
      ist->hdonly = HDONLY(node) || (ist->apps[item] == IST_HEAD);
      ist->head   = node;       /* set the new head item node */
      ist->path   = ist->buf +ist->lvlvsz;
      ist->plen   = 0;          /* clear the path */
    }
    app = ist->apps[ist->item]; /* get head item appearance */
    if (!(app & IST_HEAD) || (ist->hdonly && (app != IST_HEAD)))
      continue;                 /* if rule is not allowed, skip it */
    s_set = node->cnts[ist->index]; /* get the item set support */
    if (s_set < s_min) {        /* if the set support is too low, */
      ist->item = -1; continue; }   /* skip this item set */
    parent = node->parent;      /* get the parent node */
    if (ist->plen > 0)          /* if there is a path, use it */
      s_sub = _getsupp(ist->head, ist->path, ist->plen);
    else if (!parent)           /* if there is no parent (root node), */
      s_sub = ist->tacnt;       /* get the number of transactions */
    else if (parent->offset >= 0)   /* if a pure vector is used */
      s_sub = parent->cnts[ID(node) -parent->offset];
    else {                      /* if an identifier map is used */
      map = parent->cnts +(n = parent->size);
      /* vec = (ISNODE**)(map +n); (unused) *//* get id. map and child vector */
      s_sub = parent->cnts[_bsearch(map, n, ID(node))];
    }                           /* find vector index and get support */
    if (s_sub < s_rule)         /* if the subset support is too low, */
      continue;                 /* get the next subset/next set */
    c = (s_sub > 0)             /* compute the rule confidence */
      ? (double)s_set/s_sub : 1;
    if (c < ist->conf -EPSILON) /* if the confidence is too low, */
      continue;                 /* get the next item subset/item set */
    if (ist->arem == EM_NONE) { /* if no add. eval. measure given, */
      v = 0; break; }           /* abort the loop (select the rule) */
    if (ist->size < 2) {        /* if rule has an empty antecedent, */
      v = 0; break; }           /* abort the loop (select the rule) */
    if (ist->tacnt <= 0)        /* if there are no transactions, */
      p_body = p_head = 1;      /* all probabilities are 1 */
    else {                      /* if there are transactions */
      p_body = (double)s_sub                           /ist->tacnt;
      p_head = (double)ist->levels[0]->cnts[ist->item] /ist->tacnt;
    }                           /* estimate prior probabilities */
    v = _evalfns[ist->arem](p_head, p_body, c);
    if (v >= ist->minval)       /* if rule value exceeds the minimal */
      break;                    /* of the add. rule eval. measure, */
  }  /* while (1) */            /* abort the loop (select rule) */
  *supp = (ist->tacnt <= 0) ? 1 /* compute the rule support */
        : ((ist->rsdef == IST_BODY) ? s_sub : s_set)
        / (double)ist->tacnt;   /* (respect the rule support def.) */
  if (lift)                     /* compute and store the lift value */
    *lift = (c *ist->tacnt)/(double)ist->levels[0]->cnts[ist->item];

  /* --- build rule --- */
  if (node->offset >= 0) item = node->offset +ist->index;
  else                   item = node->cnts[node->size +ist->index];
  i = ist->size;                /* get the current item and */
  if (item != ist->item)        /* if this item is not the head, */
    rule[--i] = item;           /* add it to the rule body */
  while (node->parent) {        /* traverse the path to the root */
    if (ID(node) != ist->item)  /* and add all items on this */
      rule[--i] = ID(node);     /* path to the rule body */
    node = node->parent;        /* (except the head of the rule) */
  }
  rule[0] = ist->item;          /* set the head of the rule, */
  *conf   = c;                  /* the rule confidence, and */
  if (aval) *aval = v;          /* the value of the add. measure */
  return ist->size;             /* return the rule size */
}  /* ist_rule() */

/*--------------------------------------------------------------------*/

int ist_hedge (ISTREE *ist, int *hedge, double *supp, double *conf)
{                               /* --- extract next hyperedge */
  int    i;                     /* loop variable */
  int    item;                  /* an item identifier */
  ISNODE *node;                 /* current item set node */
  ISNODE *head;                 /* node containing the rule head */
  /* ISNODE **vec; (unused) */                 /* child node vector of head node */
  int    *map, n;               /* identifier map and its size */
  int    *path, plen;           /* path in tree and its length */
  int    s_min;                 /* minimal support of a hyperedge */
  int    s_set;                 /* support of set    (body & head) */
  int    s_sub;                 /* support of subset (body) */

  assert(ist && hedge && supp && conf);  /* check function arguments */

  /* --- initialize --- */
  if (ist->size > ist->lvlcnt)  /* if the tree is not high enough */
    return -1;                  /* for the hyperedge size, abort */
  s_min = (int)ceil(ist->tacnt *ist->supp); /* get minimal support */
  if (!ist->node)               /* on first hyperedge, initialize */
    ist->node = ist->levels[ist->size -1];    /* the current node */
  node = ist->node;             /* get the current item set node */

  /* --- find hyperedge --- */
  while (1) {                   /* search for a hyperedge */
    if (++ist->index >= node->size) { /* if all subsets have been */
      node = node->succ;        /* processed, go to the successor */
      if (!node) {              /* if at the end of a level, go down */
        if (++ist->size > ist->lvlcnt)
          return -1;            /* if beyond the deepest level, abort */
        node = ist->levels[ist->size -1];
      }                         /* get the 1st node of the new level */
      ist->node  = node;        /* note the new item set node and */
      ist->index = 0;           /* start with the first item set */
    }                           /* of the new item set node */
    if (node->offset >= 0) item = node->offset +ist->index;
    else                   item = node->cnts[node->size +ist->index];
    if (ist->apps[item] == IST_IGNORE)
      continue;                 /* skip items to ignore */
    s_set = node->cnts[ist->index];
    if (s_set < s_min)          /* if the set support is too low, */
      continue;                 /* skip this item set */
    head = node->parent;        /* get subset support from parent */
    if (!head)                  /* if there is no parent (root node), */
      s_sub = ist->tacnt;       /* get the total number of sets */
    else if (head->offset >= 0) /* if pure vectors are used */
      s_sub = head->cnts[ID(node) -head->offset];
    else {                      /* if an identifier map is used */
      map = head->cnts +(n = head->size);
      /* vec = (ISNODE**)(map +n); (unused) */ /* get id. map and child vector */
      s_sub = head->cnts[_bsearch(map, n, ID(node))];
    }                           /* find index and get the support */
    *conf   = (s_sub > 0)       /* compute confidence of first rule */
            ? (double)s_set/s_sub : 1;
    path    = ist->buf +ist->lvlvsz;
    *--path = ist->index +node->offset;
    plen    = 1;                /* initialize the path, */
    item    = ID(node);         /* note the next head item, and */
    while (head) {              /* traverse the path up to root */
      s_sub = _getsupp(head, path, plen);
      *conf += (s_sub > 0)      /* sum the rule confidences */
             ? (double)s_set/s_sub : 1;
      *--path = item; plen++;   /* store the previous head item */
      item = ID(head);          /* in the path (extend path) */
      head = head->parent;      /* and go to the parent node */
    }                           /* (get the next rule head) */
    *conf /= ist->size;         /* average the rule confidences */
    if (*conf >= ist->minval) break;
  }  /* while(1) */             /* if confidence suffices, abort loop */
  *supp = (ist->tacnt > 0)      /* compute the hyperedge support */
        ? (double)s_set/ist->tacnt : 1;

  /* --- build hyperedge --- */
  i = ist->size -1;             /* store the first item */
  if (node->offset >= 0) hedge[i] = ist->index +node->offset;
  else                   hedge[i] = node->cnts[node->size +ist->index];
  while (node->parent) {        /* while not at the root node */
    hedge[--i] = ID(node);      /* add item to the hyperedge */
    node = node->parent;        /* and go to the parent node */
  }
  return ist->size;             /* return the hyperedge size */
}  /* ist_hedge() */

/*--------------------------------------------------------------------*/
#ifndef NDEBUG

static void _showtree (ISNODE *node, int level)
{                               /* --- show subtree */
  int    i, k;                  /* loop variables, buffer */
  int    *map, n;               /* identifier map and its size */
  int    c;                     /* number of children */
  ISNODE **vec;                 /* vector of child nodes */

  assert(node && (level >= 0)); /* check the function arguments */
  c = node->chcnt & ~F_SKIP;    /* get the number of children */
  if      (c <= 0)              /* if there are no children, */
    vec = NULL;                 /* clear the child vector variable */
  else if (node->offset >= 0)   /* if a pure vector is used */
/*    vec = (ISNODE**)(node->cnts +node->size); */
    vec = get_vec(node);
  else {                        /* if an identifier map is used */
    map = node->cnts +(n = node->size);
    vec = (ISNODE**)(map +n);   /* get id. map and child vector */
    if (c < n)                  /* if a secondary id. map exists, */
      map = (int*)(vec +(n = c));      /* get this identifier map */
  }                             /* get child access variables */
  for (i = 0; i < node->size; i++) {
    for (k = level; --k >= 0; ) /* indent and print */
      printf("   ");            /* item identifier and counter */
    if (node->offset >= 0) k = node->offset +i;
    else                   k = node->cnts[node->size +i];
    printf("%d: %d\n", k, node->cnts[i]);
    if (!vec) continue;         /* check whether there are children */
    if (node->offset >= 0) k -= ID(vec[0]);
    else                   k = _bsearch(map, n, k);
    if ((k >= 0) && (k < c) && vec[k])
      _showtree(vec[k], level +1);
  }                             /* show subtree recursively */
}  /* _showtree() */

/*--------------------------------------------------------------------*/

void ist_show (ISTREE *ist)
{                               /* --- show an item set tree */
  assert(ist);                  /* check the function argument */
  _showtree(ist->levels[0], 0); /* show nodes recursively */
  printf("total: %d\n", ist->tacnt);
}  /* ist_show() */             /* print number of transactions */

#endif
