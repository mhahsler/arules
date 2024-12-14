/*----------------------------------------------------------------------
  File    : tract.c
  Contents: item and transaction management
  Author  : Christian Borgelt
  History : 14.02.1996 file created as apriori.c
            24.06.1996 function _get_item optimized
            01.07.1996 adapted to modified symtab module
            04.01.1998 scan functions moved to module 'tfscan'
            09.06.1998 vector enlargement modified
            20.06.1998 adapted to changed st_create function
            07.08.1998 bug in function _get_tract (is_read) fixed
            08.08.1998 item appearances added
            17.08.1998 item sorting and recoding added
            02.09.1998 several assertions added
            05.02.1999 long int changed to int
            22.10.1999 bug in item appearances reading fixed
            11.11.1999 adapted to name/identifier maps
            01.12.1999 check of item appearance added to sort function
            15.03.2000 removal of infrequent items added
            14.07.2001 adapted to modified module tfscan
            27.12.2001 item functions made a separate module
            18.11.2001 transaction functions made a separate module
            28.12.2001 first version of this module completed
            12.01.2002 empty field at end of record reported as error
            06.02.2002 item sorting reversed (ascending order)
            19.02.2002 transaction tree functions added
            17.07.2003 functions is_filter, ta_filter, tas_filter added
            15.08.2003 bug in function tat_delete fixed
            21.08.2003 parameter 'heap' added to tas_sort, tat_create
            20.09.2003 empty transactions in input made possible
            18.12.2003 padding for 64 bit architecture added
            26.02.2004 item frequency counting moved to is_read
            12/9/2013 fixed 64-bit address alignment (MFH)
----------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "tract.h"
#include "vecops.h"
#ifdef STORAGE
#include "storage.h"
#endif


/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
#define BLKSIZE  256            /* block size for enlarging vectors */

/*----------------------------------------------------------------------
  Constants
----------------------------------------------------------------------*/
/* --- item appearance indicators --- */
static const char *i_body[] = { /* item to appear in bodies only */
  "i",  "in",  "a", "ante", "antecedent", "b", "body", NULL };
static const char *i_head[] = { /* item to appear in heads only */
  "o",  "out", "c", "cons", "consequent", "h", "head", NULL };
static const char *i_both[] = { /* item to appear in both */
  "io", "inout", "ac", "bh", "both",                   NULL };
static const char *i_ignore[] ={/* item to ignore */
  "n", "neither", "none", "ign", "ignore", "-",        NULL };

/*----------------------------------------------------------------------
  Auxiliary Functions
----------------------------------------------------------------------*/

static int _appcode (const char *s)
{                               /* --- get appearance indicator code */
  const char **p;               /* to traverse indicator list */

  assert(s);                    /* check the function argument */
  for (p = i_body;   *p; p++)   /* check 'body' indicators */
    if (strcmp(s, *p) == 0) return APP_BODY;
  for (p = i_head;   *p; p++)   /* check 'head' indicators */
    if (strcmp(s, *p) == 0) return APP_HEAD;
  for (p = i_both;   *p; p++)   /* check 'both' indicators */
    if (strcmp(s, *p) == 0) return APP_BOTH;
  for (p = i_ignore; *p; p++)   /* check 'ignore' indicators */
    if (strcmp(s, *p) == 0) return APP_NONE;
  return -1;                    /* if none found, return error code */
}  /* _appcode() */

/*--------------------------------------------------------------------*/

static int _get_item (ITEMSET *iset, FILE *file)
{                               /* --- read an item */
  int  d;                       /* delimiter type */
  char *buf;                    /* read buffer */
  ITEM *item;                   /* pointer to item */
  int  *vec;                    /* new item vector */
  int  size;                    /* new item vector size */

  assert(iset && file);         /* check the function arguments */
  d   = tfs_getfld(iset->tfscan, file, NULL, 0);
  buf = tfs_buf(iset->tfscan);  /* read the next field (item name) */
  if ((d < 0) || (buf[0] == '\0')) return d;
  item = nim_byname(iset->nimap, buf);
  if (!item) {                  /* look up the name in name/id map */
    if (iset->app == APP_NONE)  /* if new items are to be ignored, */
      return d;                 /* do not register the item */
    item = nim_add(iset->nimap, buf, sizeof(ITEM));
    if (!item) return E_NOMEM;  /* add the new item to the map, */
    item->frq = item->xfq = 0;  /* initialize the frequency counters */
    item->app = iset->app;      /* (occurrence and sum of t.a. sizes) */
  }                             /* and set the appearance indicator */
  size = iset->vsz;             /* get the item vector size */
  if (iset->cnt >= size) {      /* if the item vector is full */
    size += (size > BLKSIZE) ? (size >> 1) : BLKSIZE;
    vec   = (int*)realloc(iset->items, size *sizeof(int));
    if (!vec) return E_NOMEM;   /* enlarge the item vector */
    iset->items = vec; iset->vsz = size;
  }                             /* set the new vector and its size */
  iset->items[iset->cnt++] = item->id;
  return d;                     /* add the item to the transaction */
}  /* _get_item() */            /* and return the delimiter type */

/*--------------------------------------------------------------------*/

static int _asccmp (const void *p1, const void *p2, void *data)
{                               /* --- compare item frequencies */
  if (((const ITEM*)p1)->app == APP_NONE)
    return (((const ITEM*)p2)->app == APP_NONE) ? 0 : 1;
  if (((const ITEM*)p2)->app == APP_NONE) return -1;
  /* 64-bit fix 
  if (((const ITEM*)p1)->frq < (int)data)
    return (((const ITEM*)p2)->frq < (int)data) ? 0 : 1;
  if (((const ITEM*)p2)->frq < (int)data) return -1;
  */
  if (((const ITEM*)p1)->frq < *(int *)data)
    return (((const ITEM*)p2)->frq < *(int *)data) ? 0 : 1;
  if (((const ITEM*)p2)->frq < *(int *)data) return -1;
  
  if (((const ITEM*)p1)->frq > ((const ITEM*)p2)->frq) return  1;
  if (((const ITEM*)p1)->frq < ((const ITEM*)p2)->frq) return -1;
  return 0;                     /* return sign of frequency diff. */
}  /* _asccmp() */

/*--------------------------------------------------------------------*/

static int _descmp (const void *p1, const void *p2, void *data)
{                               /* --- compare item frequencies */
  if (((const ITEM*)p1)->app == APP_NONE)
    return (((const ITEM*)p2)->app == APP_NONE) ? 0 : 1;
  if (((const ITEM*)p2)->app == APP_NONE) return -1;
  if (((const ITEM*)p1)->frq > ((const ITEM*)p2)->frq) return -1;
  if (((const ITEM*)p1)->frq < ((const ITEM*)p2)->frq) return  1;
  return 0;                     /* return sign of frequency diff. */
}  /* _descmp() */

/*--------------------------------------------------------------------*/

static int _asccmpx (const void *p1, const void *p2, void *data)
{                               /* --- compare item frequencies */
  if (((const ITEM*)p1)->app == APP_NONE)
    return (((const ITEM*)p2)->app == APP_NONE) ? 0 : 1;
  if (((const ITEM*)p2)->app == APP_NONE) return -1;
  /* 64-bit fix
  if (((const ITEM*)p1)->frq < (int)data)
    return (((const ITEM*)p2)->frq < (int)data) ? 0 : 1;
  if (((const ITEM*)p2)->frq < (int)data) return -1;
  */
  if (((const ITEM*)p1)->frq < *(int*)data)
    return (((const ITEM*)p2)->frq < *(int*)data) ? 0 : 1;
  if (((const ITEM*)p2)->frq < *(int*)data) return -1;
  
  if (((const ITEM*)p1)->xfq > ((const ITEM*)p2)->xfq) return  1;
  if (((const ITEM*)p1)->xfq < ((const ITEM*)p2)->xfq) return -1;
  return 0;                     /* return sign of frequency diff. */
}  /* _asccmpx() */

/*--------------------------------------------------------------------*/

static int _descmpx (const void *p1, const void *p2, void *data)
{                               /* --- compare item frequencies */
  if (((const ITEM*)p1)->app == APP_NONE)
    return (((const ITEM*)p2)->app == APP_NONE) ? 0 : 1;
  if (((const ITEM*)p2)->app == APP_NONE) return -1;
  /* 64-bit fix
  if (((const ITEM*)p1)->frq < (int)data)
    return (((const ITEM*)p2)->frq < (int)data) ? 0 : 1;
  if (((const ITEM*)p2)->frq < (int)data) return -1;
  */  
  if (((const ITEM*)p1)->frq < *(int*)data)
    return (((const ITEM*)p2)->frq < *(int*)data) ? 0 : 1;
  if (((const ITEM*)p2)->frq < *(int*)data) return -1;
  
  if (((const ITEM*)p1)->xfq > ((const ITEM*)p2)->xfq) return -1;
  if (((const ITEM*)p1)->xfq < ((const ITEM*)p2)->xfq) return  1;
  return 0;                     /* return sign of frequency diff. */
}  /* _descmpx() */

/*----------------------------------------------------------------------
  Item Set Functions
----------------------------------------------------------------------*/

ITEMSET* is_create (void)
{                               /* --- create an item set */
  ITEMSET *iset;                /* created item set */

  iset = malloc(sizeof(ITEMSET));
  if (!iset) return NULL;       /* create an item set */
  iset->tfscan = tfs_create();  /* and its components */
  iset->nimap  = nim_create(0, 0, (HASHFN*)0, (SYMFN*)0);
  iset->items  = (int*)malloc(BLKSIZE *sizeof(int));
  if (!iset->tfscan || !iset->nimap || !iset->items) {
    is_delete(iset); return NULL; }
  iset->app    = APP_BOTH;      /* initialize the other fields */
  iset->vsz    = BLKSIZE;
  iset->cnt    = 0;
  iset->chars[0] = ' ';  iset->chars[1] = ' ';
  iset->chars[2] = '\n'; iset->chars[3] = '\0';
  return iset;                  /* return the created item set */
}  /* is_create() */

/*--------------------------------------------------------------------*/

void is_delete (ITEMSET *iset)
{                               /* --- delete an item set */
  assert(iset);                 /* check the function argument */
  if (iset->items)  free(iset->items);
  if (iset->nimap)  nim_delete(iset->nimap);
  if (iset->tfscan) tfs_delete(iset->tfscan);
  free(iset);                   /* delete the components */
}  /* is_delete() */            /* and the item set body */

/*--------------------------------------------------------------------*/

void is_chars (ITEMSET *iset, const char *blanks,  const char *fldseps,
                              const char *recseps, const char *cominds)
{                               /* --- set special characters */
  assert(iset);                 /* check the function argument */
  if (blanks)                   /* set blank characters */
    iset->chars[0] = tfs_chars(iset->tfscan, TFS_BLANK,  blanks);
  if (fldseps)                  /* set field separators */
    iset->chars[1] = tfs_chars(iset->tfscan, TFS_FLDSEP, fldseps);
  if (recseps)                  /* set record separators */
    iset->chars[2] = tfs_chars(iset->tfscan, TFS_RECSEP, recseps);
  if (cominds)                  /* set comment indicators */
    tfs_chars(iset->tfscan, TFS_COMMENT, cominds);
}  /* is_chars() */

/*--------------------------------------------------------------------*/

int is_item (ITEMSET *iset, const char *name)
{                               /* --- get an item identifier */
  ITEM *item = nim_byname(iset->nimap, name);
  return (item) ? item->id :-1; /* look up the given name */
}  /* is_item() */              /* in the name/identifier map */

/*--------------------------------------------------------------------*/

int is_readapp (ITEMSET *iset, FILE *file)
{                               /* --- read appearance indicators */
  int  d;                       /* delimiter type */
  char *buf;                    /* read buffer */
  ITEM *item;                   /* to access the item data */

  assert(iset && file);         /* check the function arguments */
  if (tfs_skip(iset->tfscan, file) < 0)
    return E_FREAD;             /* skip leading comments */
  buf = tfs_buf(iset->tfscan);  /* read the first record (one field) */
  d   = tfs_getfld(iset->tfscan, file, NULL, 0);
  if (d <  0)           return E_FREAD;
  if (d >= TFS_FLD)     return E_FLDCNT;
  iset->app = _appcode(buf);    /* get default appearance code */
  if (iset->app < 0)    return E_UNKAPP;
  while (d > TFS_EOF) {         /* read item/indicator pairs */
    if (tfs_skip(iset->tfscan, file) < 0)
      return E_FREAD;           /* skip more comments */
    d = tfs_getfld(iset->tfscan, file, NULL, 0);
    if (d <= TFS_EOF)           /* read the next item */
      return (d < 0) ? E_FREAD : 0;
    if (buf[0] == '\0')         /* check for end of file */
      return E_ITEMEXP;         /* and for a missing item */
    item = nim_add(iset->nimap, buf, sizeof(ITEM));
    if (item == EXISTS) return E_DUPITEM;  /* add the new item */
    if (item == NULL)   return E_NOMEM;    /* to the name/id map */
    item->frq = 0;              /* clear the frequency counters */
    item->xfq = 0;              /* (occurrence and sum of t.a. sizes) */
    if (d < TFS_FLD)    return E_APPEXP;
    d = tfs_getfld(iset->tfscan, file, NULL, 0);
    if (d <  0)         return E_FREAD;
    if (d >= TFS_FLD)   return E_FLDCNT;
    item->app = _appcode(buf);  /* get the appearance indicator */
    if (item->app <  0)  return E_UNKAPP;
  }
  return 0;                     /* return 'ok' */
}  /* is_readapp() */

/*--------------------------------------------------------------------*/

int is_read (ITEMSET *iset, FILE *file)
{                               /* --- read a transaction */
  int  i, d;                    /* loop variable, delimiter type */
  char *buf;                    /* read buffer */
  ITEM *item;                   /* pointer to item */

  assert(iset && file);         /* check the function arguments */
  iset->cnt = 0;                /* initialize the item counter */
  if (tfs_skip(iset->tfscan, file) < 0)
    return E_FREAD;             /* skip leading comments */
  d   = _get_item(iset, file);  /* read the first item and */
  buf = tfs_buf(iset->tfscan);  /* get the read buffer */
  if ((d      == TFS_EOF)       /* if at the end of the file */
  &&  (buf[0] == '\0'))         /* and no item has been read, */
    return 1;                   /* return 'end of file' */
  while ((d      == TFS_FLD)    /* read the other items */
  &&     (buf[0] != '\0'))      /* of the transaction */
    d = _get_item(iset, file);  /* up to the end of the record */
  if (d < TFS_EOF) return d;    /* check for a read error */
  if ((buf[0] == '\0') && (d == TFS_FLD) && (iset->cnt > 0))
    return E_ITEMEXP;           /* check for an empty field */
  ta_sort(iset->items, iset->cnt); /* prepare the transaction */
  iset->cnt = ta_unique(iset->items, iset->cnt);
  for (i = iset->cnt; --i >= 0; ) {
    item = nim_byid(iset->nimap, iset->items[i]);
    item->frq += 1;             /* count the item and */
    item->xfq += iset->cnt;     /* sum the transaction sizes */
  }                             /* as an importance indicator */
  return 0;                     /* return 'ok' */
}  /* is_read() */

/*--------------------------------------------------------------------*/

int is_recode (ITEMSET *iset, int minfrq, int dir, int *map, int mode, int fullS)
{                               /* --- recode items w.r.t. frequency */
  int  i, k, n, t;              /* loop variables, buffer */
  ITEM *item;                   /* to traverse the items */
  SYMCMPFN *cmp;                /* comparison function */

  assert(iset);                 /* check the function arguments */
  if      (dir >  1) cmp = _asccmpx;  /* get the appropriate */
  else if (dir >= 0) cmp = _asccmp;   /* comparison function */
  else if (dir > -2) cmp = _descmp;   /* and sort the items */
  else               cmp = _descmpx;  /* w.r.t. their frequency */

/* 29.9.2005: problems with casting void *data to int on 64-bit architectures
 * reported by Brian Ripley.
 * We pass a reference now instead of casting the pointer directly to int
 * required changes in _asccmp, _asccmpx and _descmpx
 */
  
  /* nim_sort(iset->nimap, cmp, (void*)minfrq, map, 1); */
  nim_sort(iset->nimap, cmp, &minfrq, map, 1);

  for (n = nim_cnt(iset->nimap); --n >= 0; ) {
    item = (ITEM*)nim_byid(iset->nimap, n);
    if ((item->frq < minfrq) || (mode && (item->frq == fullS)))     /* determine frequent items and */
      item->app = APP_NONE;     /* set all others to 'ignore' */
    else if (item->app != APP_NONE)
      break;                    /* in addition, skip all items */
  }                             /* that have been set to 'ignore' */
  if (map) {                    /* if a map vector is provided */
    for (i = k = 0; i < iset->cnt; i++) {
      t = map[iset->items[i]];  /* traverse the current transaction */
      if (t <= n) iset->items[k++] = t;
    }                           /* recode all items and */
    iset->cnt = k;              /* delete all items to ignore */
    ta_sort(iset->items, k);    /* resort the items */
  }
  return n+1;                   /* return number of frequent items */
}  /* is_recode() */

/*--------------------------------------------------------------------*/

int is_filter (ITEMSET *iset, const char *marks)
{                               /* --- filter items in transaction */
  return iset->cnt = ta_filter(iset->items, iset->cnt, marks);
}  /* is_filter() */

/*----------------------------------------------------------------------
  Transaction Functions
----------------------------------------------------------------------*/

int ta_unique (int *items, int n)
{                               /* --- remove duplicate items */
  int *s, *d;                   /* to traverse the item vector */

  assert(items && (n >= 0));    /* check the function arguments */
  if (n <= 1) return n;         /* check for 0 or 1 item */
  for (d = s = items; --n > 0;) /* traverse the sorted vector */
    if (*++s != *d) *++d = *s;  /* and remove duplicate items */ 
  return (int)(++d -items);     /* return the new number of items */
}  /* ta_unique() */

/*--------------------------------------------------------------------*/

int ta_filter (int *items, int n, const char *marks)
{                               /* --- filter items in a transaction */
  int i, k;                     /* loop variables */

  assert(items && (n >= 0));    /* check the function arguments */
  for (i = k = 0; i < n; i++)   /* remove all unmarked items */
    if (marks[items[i]]) items[k++] = items[i];
  return k;                     /* return the new number of items */
}  /* ta_filter() */

/*--------------------------------------------------------------------*/

static int ta_cmp (const void *p1, const void *p2, void *data)
{                               /* --- compare transactions */
  int       k, k1, k2;          /* loop variable, counters */
  const int *i1, *i2;           /* to traverse the item identifiers */

  assert(p1 && p2);             /* check the function arguments */
  i1 = ((const TRACT*)p1)->items;
  i2 = ((const TRACT*)p2)->items;
  k1 = ((const TRACT*)p1)->cnt; /* get the item vectors */
  k2 = ((const TRACT*)p2)->cnt; /* and the numbers of items */
  for (k  = (k1 < k2) ? k1 : k2; --k >= 0; i1++, i2++) {
    if (*i1 > *i2) return  1;   /* compare corresponding items */
    if (*i1 < *i2) return -1;   /* and abort the comparison */
  }                             /* if one of them is greater */
  if (k1 > k2) return  1;       /* if one of the transactions */
  if (k1 < k2) return -1;       /* is not empty, it is greater */
  return 0;                     /* otherwise the two trans. are equal */
}  /* ta_cmp() */

/*--------------------------------------------------------------------*/

static int ta_cmpx (const TRACT *ta, const int *items, int n)
{                               /* --- compare transactions */
  int       k, m;               /* loop variable, counter */
  const int *p;                 /* to traverse the item identifiers */

  assert(ta && items);          /* check the function arguments */
  p = ta->items; m = ta->cnt;   /* traverse the item vector */
  m = ta->cnt;
  for (k = (n < m) ? n : m; --k >= 0; p++, items++) {
    if (*p > *items) return  1; /* compare corresponding items */
    if (*p < *items) return -1; /* and abort the comparison */
  }                             /* if one of them is greater */
  if (m > n) return  1;         /* if one of the transactions */
  if (m < n) return -1;         /* is not empty, it is greater */
  return 0;                     /* otherwise the two trans. are equal */
}  /* ta_cmpx() */

/*----------------------------------------------------------------------
  Transaction Set Functions
----------------------------------------------------------------------*/

TASET* tas_create (ITEMSET *itemset)
{                               /* --- create a transaction set */
  TASET *taset;                 /* created transaction set */

  assert(itemset);              /* check the function argument */
  taset = malloc(sizeof(TASET));
  if (!taset) return NULL;      /* create a transaction set */
  taset->itemset = itemset;     /* and store the item set */
  taset->cnt     = taset->vsz = taset->max = taset->total = 0;
  taset->tracts  = NULL;        /* initialize the other fields */
  return taset;                 /* return the created t.a. set */
}  /* tas_create() */

/*--------------------------------------------------------------------*/

void tas_delete (TASET *taset, int delis)
{                               /* --- delete a transaction set */
  assert(taset);                /* check the function argument */
  if (taset->tracts) {          /* if there are loaded transactions */
    while (--taset->cnt >= 0)   /* traverse the transaction vector */
      free(taset->tracts[taset->cnt]);
    free(taset->tracts);        /* delete all transactions */
  }                             /* and the transaction vector */
  if (delis && taset->itemset) is_delete(taset->itemset);
  free(taset);                  /* delete the item set and */
}  /* tas_delete() */           /* the transaction set body */

/*--------------------------------------------------------------------*/

int tas_add (TASET *taset, const int *items, int n)
{                               /* --- add a transaction */
  TRACT *ta;                    /* new transaction */
  int   *p;                     /* to traverse the transaction */
  TRACT **vec;                  /* new transaction vector */
  int   size;                   /* new transaction vector size */

  assert(taset);                /* check the function arguments */
  size = taset->vsz;            /* get the transaction vector size */
  if (taset->cnt >= size) {     /* if the transaction vector is full */
    size += (size > BLKSIZE) ? (size >> 1) : BLKSIZE;
    vec   = (TRACT**)realloc(taset->tracts, size *sizeof(TRACT*));
    if (!vec) return -1;        /* enlarge the transaction vector */
    taset->tracts = vec; taset->vsz = size;
  }                             /* set the new vector and its size */
  if (!items) {                 /* if no transaction is given */
    items = is_tract(taset->itemset);
    n     = is_tsize(taset->itemset);
  }                             /* get it from the item set */
  ta = (TRACT*)malloc(sizeof(TRACT) + (n) *sizeof(int));
  if (!ta) return -1;           /* create a new transaction */
  taset->tracts[taset->cnt++]  = ta;
  if (n > taset->max)           /* store the transaction and */
    taset->max = n;             /* update maximal transaction size */
  taset->total += n;            /* sum the number of items */
  for (p = ta->items +(ta->cnt = n); --n >= 0; )
    *--p = items[n];            /* copy the items of the t.a. */
  return 0;                     /* return 'ok' */
}  /* tas_add() */

/*--------------------------------------------------------------------*/

void tas_recode (TASET *taset, int *map, int cnt)
{                               /* --- recode items */
  int   i, k, n, x;             /* loop variables, buffer */
  TRACT *t;                     /* to traverse the transactions */
  int   *p;                     /* to traverse the item identifiers */

  assert(taset && map);         /* check the function arguments */
  taset->max = taset->total = 0;/* clear the maximal size and total */
  for (n = taset->cnt; --n >= 0; ) {
    t = taset->tracts[n];       /* traverse the transactions and */
    p = t->items;               /* the items of each transaction */
    for (i = k = 0; i < t->cnt; i++) {
      x = map[p[i]];            /* recode the items and */
      if (x < cnt) p[k++] = x;  /* remove superfluous items */
    }                           /* from the transaction */
    if (k > taset->max)         /* update the max. transaction size */
      taset->max = k;           /* with the new size of the t.a. */
    taset->total += k;          /* sum the number of items */
    ta_sort(t->items, t->cnt = k);
  }                             /* resort the item identifiers */
}  /* tas_recode() */

/*--------------------------------------------------------------------*/

int tas_filter (TASET *taset, const char *marks)
{                               /* --- filter items in a trans. set */
  int   i, max = 0;             /* loop variable, max. num. of items */
  TRACT *t;                     /* to traverse the transactions */

  assert(taset && marks);       /* check the function arguments */
  taset->total = 0;             /* clear the total number of items */
  for (i = taset->cnt; --i >= 0; ) {
    t = taset->tracts[i];       /* traverse the transactions */
    t->cnt = ta_filter(t->items, t->cnt, marks);
    if (t->cnt > max) max = t->cnt;
    taset->total += t->cnt;     /* filter each transaction and */
  }                             /* update maximal size and total */
  return max;                   /* return maximum number of items */
}  /* tas_filter() */

/*--------------------------------------------------------------------*/

void tas_sort (TASET *taset, int heap)
{                               /* --- sort a transaction set */
  assert(taset);                /* check the function argument */
  if (heap) v_heapsort(taset->tracts, taset->cnt, ta_cmp, NULL);
  else      v_sort    (taset->tracts, taset->cnt, ta_cmp, NULL);
}  /* tas_sort() */

/*--------------------------------------------------------------------*/

int tas_occur (TASET *taset, const int *items, int n)
{                               /* --- count transaction occurrences */
  int l, r, m, k = taset->cnt;  /* index variables */

  assert(taset && items);       /* check the function arguments */
  for (r = m = 0; r < k; ) {    /* find right boundary */
    m = (r + k) >> 1;           /* by a binary search */
    if (ta_cmpx(taset->tracts[m], items, n) > 0) k = m;
    else                                         r = m+1;
  }
  for (l = m = 0; l < k; ) {    /* find left boundary */
    m = (l + k) >> 1;           /* by a binary search */
    if (ta_cmpx(taset->tracts[m], items, n) < 0) l = m+1;
    else                                         k = m;
  }
  return r -l;                  /* compute the number of occurrences */
}  /* tas_occur() */

/*--------------------------------------------------------------------*/

/*----------------------------------------------------------------------
  Transaction Tree Functions
----------------------------------------------------------------------*/

TATREE* _create (TRACT **tracts, int cnt, int index)
{                               /* --- recursive part of tat_create() */
  int    i, k, t;               /* loop variables, buffer */
  int    item, n;               /* item and item counter */
  TATREE *tat;                  /* created transaction tree */
  TATREE **vec;                 /* vector of child pointers */

  assert(tracts                 /* check the function arguments */
     && (cnt >= 0) && (index >= 0));
  if (cnt <= 1) {               /* if only one transaction left */
    n   = (cnt > 0) ? (*tracts)->cnt -index : 1;
    tat = (TATREE*)malloc(sizeof(TATREE) +(n) *sizeof(int));
    if (!tat) return NULL;      /* create a transaction tree node */
    tat->cnt  = cnt;            /* and initialize its fields */
    tat->size = -n;
    tat->max  =  n;
    while (--n >= 0) tat->items[n] = (*tracts)->items[index +n];
    return tat;
  }
  for (k = cnt; (--k >= 0) && ((*tracts)->cnt <= index); )
    tracts++;                   /* skip t.a. that are too short */
  n = 0; item = -1;             /* init. item and item counter */
  for (tracts += i = ++k; --i >= 0; ) {
    t = (*--tracts)->items[index]; /* traverse the transactions */
    if (t != item) { item = t; n++; }
  }                             /* count the different items */
  
  i = tat_align(n);  /* 64 bit pointer alignment */
  tat = (TATREE*)malloc(sizeof(TATREE) + (i) *sizeof(int)
                                       + n     *sizeof(TATREE*));
  if (!tat) return NULL;        /* create a transaction tree node */
  tat->cnt  = cnt;              /* and initialize its fields */
  tat->size = n;
  tat->max  = 0;
  if (n <= 0) return tat;       /* if t.a. are fully captured, abort */
  vec  = tat_vec(tat);
  item = tracts[--k]->items[index];
  for (tracts += i = k; --i >= 0; ) {
    t = (*--tracts)->items[index];     /* traverse the transactions, */
    if (t == item) continue;    /* but skip those with the same item */
    tat->items[--n] = item; item = t;
    vec[n] = _create(tracts+1, k-i, index+1);
    if (!vec[n]) break;         /* note the item identifier */
    t = vec[n]->max +1; if (t > tat->max) tat->max = t;
    k = i;                      /* recursively create subtrees */
  }                             /* and adapt the section end index */
  if (i < 0) {                  /* if child creation was successful */
    tat->items[--n] = item;     /* node the last item identifier */
    vec[n] = _create(tracts, k+1, index+1);
    if (vec[n]) {               /* create the last child */
      t = vec[n]->max +1; if (t > tat->max) tat->max = t;
      return tat;               /* return the created */
    }                           /* transaction tree */
  }                             
  for (i = tat->size; --i > n; ) tat_delete(vec[i]);
  free(tat);                    /* on error delete created subtrees */
  return NULL;                  /* and the transaction tree node */
}  /* _create() */

/*--------------------------------------------------------------------*/

TATREE* tat_create (TASET *taset, int heap)
{                               /* --- create a transactions tree */
  assert(taset);                /* check the function argument */
  if (heap) v_heapsort(taset->tracts, taset->cnt, ta_cmp, NULL);
  else      v_sort    (taset->tracts, taset->cnt, ta_cmp, NULL);
  return _create(taset->tracts, taset->cnt, 0);
}  /* tat_create() */

/*--------------------------------------------------------------------*/

void tat_delete (TATREE *tat)
{                               /* --- delete a transaction tree */
  int    i;                     /* loop variable */
  TATREE **vec;                 /* vector of child nodes */

  assert(tat);                  /* check the function argument */
  vec = tat_vec(tat);
  for (i = tat->size; --i >= 0; )
    tat_delete(vec[i]);         /* recursively delete the subtrees */
  free(tat);                    /* and the tree node itself */
}  /* tat_delete() */

/*--------------------------------------------------------------------*/

TATREE* tat_child (TATREE *tat, int index)
{                               /* --- go to a child node */

  assert(tat                    /* check the function arguments */
     && (index >= 0) && (index < tat->size));
  TATREE ** children = tat_vec(tat);
  return children[index];
}  /* tat_child */              /* return the child node/subtree */

/*--------------------------------------------------------------------*/

