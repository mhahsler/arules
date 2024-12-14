/*----------------------------------------------------------------------
  File    : symtab.c
  Contents: symbol table management
  Author  : Christian Borgelt
  History : 22.10.1995 file created
            30.10.1995 functions made independent of symbol data
            26.11.1995 symbol types and visibility levels added
            04.01.1996 st_clear added
            27.02.1996 st_insert modified
            28.06.1996 dynamic bucket vector enlargement added
            04.07.1996 bug in bucket reorganization removed
            01.04.1997 functions st_clear and st_remove combined
            29.07.1997 minor improvements
            05.08.1997 minor improvements
            16.11.1997 some comments improved
            06.02.1998 default table sizes changed
            31.05.1998 list of all symbols removed
            20.06.1998 deletion function moved to st_create
            14.07.1998 minor improvements
            01.09.1998 bug in function _sort removed, assertions added
            25.09.1998 hash function improved
            28.09.1998 types ULONG and CCHAR removed, st_stats added
            04.02.1999 long int changed to int
            10.11.1999 name/identifier map management added
            15.08.2003 renamed new to nel in st_insert (C++ compat.)
----------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include "symtab.h"
#ifdef NIMAPFN
#include "vecops.h"
#endif
#ifdef STORAGE
#include "storage.h"
#endif

/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
#define DFLT_INIT     1023      /* default initial hash table size */
#if (INT_MAX > 32767)
#define DFLT_MAX   1048575      /* default maximal hash table size */
#else
#define DFLT_MAX     16383      /* default maximal hash table size */
#endif
#define BLKSIZE        256      /* block size for identifier vector */

/*----------------------------------------------------------------------
  Default Hash Function
----------------------------------------------------------------------*/

static unsigned _hdflt (const char *name, int type)
{                               /* --- default hash function */
  register unsigned h = type;   /* hash value */

  while (*name) h ^= (h << 3) ^ (unsigned)(*name++);
  return h;                     /* compute hash value */
}  /* _hdflt() */

/*----------------------------------------------------------------------
  Auxiliary Functions
----------------------------------------------------------------------*/

static void _delsym (SYMTAB *tab)
{                               /* --- delete all symbols */
  int i;                        /* loop variable */
  STE *ste, *tmp;               /* to traverse the symbol list */

  for (i = tab->size; --i >= 0; ) {  /* traverse bucket vector */
    ste = tab->bvec[i];         /* get the next bucket list, */
    tab->bvec[i] = NULL;        /* clear the bucket vector entry, */
    while (ste) {               /* and traverse the bucket list */
      tmp = ste;                /* note the symbol to delete */
      ste = ste->succ;          /* and get the next symbol */
      if (tab->delfn) tab->delfn(tmp +1);
      free(tmp);                /* if a deletion function is given, */
    }                           /* call it and then deallocate */
  }                             /* the symbol table element */
}  /* _delsym() */

/*--------------------------------------------------------------------*/

static STE** _merge (STE *in[], int cnt[], STE **out)
{                               /* --- merge two lists into one */
  int k;                        /* index of input list */

  do {                          /* compare and merge loop */
    k = (in[0]->level > in[1]->level) ? 0 : 1;
    *out  = in[k];              /* append the element on the higher */
    out   = &(*out)->succ;      /* level to the output list and */
    in[k] = *out;               /* remove it from the input list */
  } while (--cnt[k] > 0);       /* while both lists are not empty */
  *out = in[k ^= 1];            /* append remaining elements */
  while (--cnt[k] >= 0)         /* while not at the end of the list */
    out = &(*out)->succ;        /* go to the successor element */
  in[k] = *out;                 /* set new start of the input list */
  *out  = NULL;                 /* terminate the output list and */
  return out;                   /* return new end of the output list */
}  /* _merge() */

/*--------------------------------------------------------------------*/

static STE* _sort (STE *list)
{                               /* --- sort a hash bucket list */
  STE *ste;                     /* to traverse the list, buffer */
  STE *in[2], *out[2];          /* input and output lists */
  STE **end[2];                 /* ends of output lists */
  int cnt[2];                   /* number of elements to merge */
  int run;                      /* run length in input lists */
  int rem;                      /* elements in remainder collection */
  int oid;                      /* index of output list */

  if (!list) return list;       /* empty lists need not to be sorted */
  oid = 0; out[0] = list;       /* traverse list elements */
  for (ste = list->succ; ste; ste = ste->succ)
    if ((oid ^= 1) == 0) list = list->succ;
  out[1] = list->succ;          /* split list into two equal parts */
  list   = list->succ = NULL;   /* initialize remainder collection */
  run    = 1; rem = 0;          /* and run length */
  while (out[1]) {              /* while there are two lists */
    in [0] = out[0]; in [1] = out[1];  /* move output list to input */
    end[0] = out;    end[1] = out+1;   /* reinitialize end pointers */
    out[1] = NULL;   oid    = 0;       /* start with 1st output list */
    do {                        /* merge loop */
      cnt[0]   = cnt[1] = run;  /* merge run elements from the */
      end[oid] = _merge(in, cnt, end[oid]);     /* input lists */
      oid ^= 1;                 /* toggle index of output list */
    } while (in[1]);            /* while both lists are not empty */
    if (in[0]) {                /* if there is one input list left */
      if (!list)                /* if there is no rem. collection, */
        list = in[0];           /* just note the rem. input list */
      else {                    /* if there is a rem. collection, */
        cnt[0] = run; cnt[1] = rem; in[1] = list;
        _merge(in, cnt, &list); /* merge it and the input list to */
      }                         /* get the new renmainder collection */
      rem += run;               /* there are now run more elements */
    }                           /* in the remainder collection */
    run <<= 1;                  /* double run length */
  }  /* while (out[1]) .. */
  if (rem > 0) {                /* if there is a rem. collection */
    in[0] = out[0]; cnt[0] = run;
    in[1] = list;   cnt[1] = rem;
    _merge(in, cnt, out);       /* merge it to the output list */
  }                             /* and store the result in out[0] */
  return out[0];                /* return the sorted list */
}  /* _sort() */

/*--------------------------------------------------------------------*/

static void _reorg (SYMTAB *tab)
{                               /* --- reorganize a hash table */
  int i;                        /* loop variable */
  int size;                     /* new bucket vector size */
  STE **p;                      /* new bucket vector, buffer */
  STE *ste;                     /* to traverse symbol table elements */
  STE *list = NULL;             /* list of all symbols */

  size = (tab->size << 1) +1;   /* calculate new vector size */
  if (size > tab->max)          /* if new size exceeds maximum, */
    size = tab->max;            /* set the maximal size */
  for (p = &list, i = tab->size; --i >= 0; ) {
    *p = tab->bvec[i];          /* traverse the bucket vector and */
    while (*p) p = &(*p)->succ; /* link all bucket lists together */
  }                             /* (collect symbols) */
  p = (STE**)realloc(tab->bvec, size *sizeof(STE*));
  if (!p) return;               /* enlarge bucket vector */
  tab->bvec = p;                /* set new bucket vector */
  tab->size = size;             /* and its size */
  for (p += i = size; --i >= 0; )
    *--p = NULL;                /* clear the hash buckets */
  while (list) {                /* traverse list of all symbols */
    ste = list; list = list->succ;           /* get next symbol */
    i   = tab->hash(ste->name, ste->type) %size;
    ste->succ = tab->bvec[i];   /* compute the hash bucket index */
    tab->bvec[i] = ste;         /* and insert the symbol at */
  }                             /* the head of the bucket list */
  for (i = size; --i >= 0; )    /* sort bucket lists according to */
    tab->bvec[i] = _sort(tab->bvec[i]);   /* the visibility level */
}  /* _reorg() */

/*----------------------------------------------------------------------
  Symbol Table Functions
----------------------------------------------------------------------*/

SYMTAB* st_create (int init, int max, HASHFN hash, SYMFN delfn)
{                               /* --- create a symbol table */
  SYMTAB *tab;                  /* created symbol table */

  if (init <= 0) init = DFLT_INIT;  /* check and adapt initial */
  if (max  <= 0) max  = DFLT_MAX;   /* and maximal vector size */
  tab = (SYMTAB*)malloc(sizeof(SYMTAB));
  if (!tab) return NULL;        /* allocate symbol table body */
  tab->bvec = (STE**)calloc(init, sizeof(STE*));
  if (!tab->bvec) { free(tab); return NULL; }
  tab->level = tab->cnt = 0;    /* allocate bucket vector */
  tab->size  = init;            /* and initialize fields */
  tab->max   = max;             /* of symbol table body */
  tab->hash  = (hash) ? hash : _hdflt;
  tab->delfn = delfn;
  tab->vsz   = INT_MAX;
  tab->ids   = NULL;
  return tab;                   /* return created symbol table */
}  /* st_create() */

/*--------------------------------------------------------------------*/

void st_delete (SYMTAB *tab)
{                               /* --- delete a symbol table */
  assert(tab && tab->bvec);     /* check argument */
  _delsym(tab);                 /* delete all symbols, */
  free(tab->bvec);              /* the bucket vector, */
  if (tab->ids) free(tab->ids); /* the identifier vector, */
  free(tab);                    /* and the symbol table body */
}  /* st_delete() */

/*--------------------------------------------------------------------*/

void* st_insert (SYMTAB *tab, const char *name, int type,
                 unsigned size)
{                               /* --- insert a symbol */
  unsigned h;                   /* hash value */
  int i;                        /* index of hash bucket */
  STE *ste;                     /* to traverse bucket list */
  STE *nel;                     /* new symbol table element */

  assert(tab && name            /* check the function arguments */
      && ((size >= sizeof(int)) || (tab->vsz == INT_MAX)));
  if ((tab->cnt /4 > tab->size) /* if buckets are rather full and */
  &&  (tab->size   < tab->max)) /* table does not have maximal size, */
    _reorg(tab);                /* reorganize the hash table */

  h   = tab->hash(name, type);  /* compute the hash value and */
  i   = h % tab->size;          /* the index of the hash bucket */
  ste = tab->bvec[i];           /* get first element in bucket */
  while (ste) {                 /* traverse the bucket list */
    if ((type == ste->type)     /* if symbol found */
    &&  (strcmp(name, ste->name) == 0))
      break;                    /* abort the loop */
    ste = ste->succ;            /* otherwise get the successor */
  }                             /* element in the hash bucket */
  if (ste                       /* if symbol found on current level */
  && (ste->level == tab->level))
    return EXISTS;              /* return 'symbol exists' */

#ifdef NIMAPFN                /* if name/identifier map management */
  if (tab->cnt >= tab->vsz) {   /* if the identifier vector is full */
    int vsz, **tmp;             /* (new) id vector and its size */
    vsz = tab->vsz +((tab->vsz > BLKSIZE) ? tab->vsz >> 1 : BLKSIZE);
    tmp = (int**)realloc(tab->ids, vsz *sizeof(int*));
    if (!tmp) return NULL;      /* resize the identifier vector and */
    tab->ids = tmp; tab->vsz = vsz;  /* set new vector and its size */
  }                             /* (no resizing for symbol tables */
#endif                        /* since then tab->vsz = MAX_INT) */

  nel = (STE*)malloc(sizeof(STE) +size +strlen(name) +1);
  if (!nel) return NULL;        /* allocate memory for new symbol */
  nel->name    = (char*)(nel+1) +size;         /* and organize it */
  strcpy(nel->name, name);      /* note the symbol name, */
  nel->type    = type;          /* the symbol type, and the */
  nel->level   = tab->level;    /* current visibility level */
  nel->succ    = tab->bvec[i];  /* insert new symbol at the head */
  tab->bvec[i] = nel++;         /* of the bucket list */
#ifdef NIMAPFN                /* if name/identifier maps are */
  if (tab->ids) {               /* supported and this is such a map */
    tab->ids[tab->cnt] = (int*)nel;
    *(int*)nel = tab->cnt;      /* store the new symbol */
  }                             /* in the identifier vector */
#endif                        /* and set the symbol identifier */
  tab->cnt++;                   /* increment the symbol counter */
  return nel;                   /* return pointer to data field */
}  /* st_insert() */

/*--------------------------------------------------------------------*/

int st_remove (SYMTAB *tab, const char *name, int type)
{                               /* --- remove a symbol/all symbols */
  int i;                        /* index of hash bucket */
  STE **p, *ste;                /* to traverse bucket list */

  assert(tab);                  /* check for a valid symbol table */

  /* --- remove all symbols --- */
  if (!name) {                  /* if no symbol name given */
    _delsym(tab);               /* delete all symbols */
    tab->cnt = tab->level = 0;  /* reset visibility level */
    return 0;                   /* and symbol counter */
  }                             /* and return 'ok' */

  /* --- remove one symbol --- */
  i = tab->hash(name, type) % tab->size;
  p = tab->bvec +i;             /* compute index of hash bucket */
  while (*p) {                  /* and traverse bucket list */
    if (((*p)->type == type)    /* if symbol found */
    &&  (strcmp(name, (*p)->name) == 0))
      break;                    /* abort loop */
    p = &(*p)->succ;            /* otherwise get successor */
  }                             /* in hash bucket */
  ste = *p;                     /* if the symbol does not exist, */
  if (!ste) return -1;          /* abort the function */
  *p = ste->succ;               /* remove symbol from hash bucket */
  if (tab->delfn) tab->delfn(ste +1);   /* delete user data */
  free(ste);                    /* and symbol table element */
  tab->cnt--;                   /* decrement symbol counter */
  return 0;                     /* return 'ok' */
}  /* st_remove() */

/*--------------------------------------------------------------------*/

void* st_lookup (SYMTAB *tab, const char *name, int type)
{                               /* --- look up a symbol */
  int i;                        /* index of hash bucket */
  STE *ste;                     /* to traverse bucket list */

  assert(tab && name);          /* check arguments */
  i   = tab->hash(name, type) % tab->size;
  ste = tab->bvec[i];           /* compute index of hash bucket */
  while (ste) {                 /* and traverse bucket list */
    if ((ste->type == type)     /* if symbol found */
    &&  (strcmp(name, ste->name) == 0))
      return ste +1;            /* return pointer to assoc. data */
    ste = ste->succ;            /* otherwise get successor */
  }                             /* in hash bucket */
  return NULL;                  /* return 'not found' */
}  /* st_lookup() */

/*--------------------------------------------------------------------*/

void st_endblk (SYMTAB *tab)
{                               /* --- remove one visibility level */
  int i;                        /* loop variable */
  STE *ste, *tmp;               /* to traverse bucket lists */

  assert(tab);                  /* check for a valid symbol table */
  if (tab->level <= 0) return;  /* if on level 0, abort */
  for (i = tab->size; --i >= 0; ) {  /* traverse bucket vector */
    ste = tab->bvec[i];         /* get next bucket list */
    while (ste                  /* and remove all symbols */
    &&    (ste->level >= tab->level)) {  /* of higher level */
      tmp = ste;                /* note symbol and */
      ste = ste->succ;          /* get successor */
      if (tab->delfn) tab->delfn(tmp +1);
      free(tmp);                /* delete user data and */
      tab->cnt--;               /* symbol table element */
    }                           /* and decrement symbol counter */
    tab->bvec[i] = ste;         /* set new start of bucket list */
  }
  tab->level--;                 /* go up one level */
}  /* st_endblk() */

/*--------------------------------------------------------------------*/
/*----------------------------------------------------------------------
  Name/Identifier Map Functions
----------------------------------------------------------------------*/
#ifdef NIMAPFN

NIMAP* nim_create (int init, int max, HASHFN hash, SYMFN delfn)
{                               /* --- create a name/identifier map */
  NIMAP *nim;                   /* created name/identifier map */

  nim = st_create(init, max, hash, delfn);
  if (!nim) return NULL;        /* create a name/identifier map */
  nim->vsz = 0;                 /* and clear the id. vector size */
  return nim;                   /* return created name/id map */
}  /* nim_create() */

/*--------------------------------------------------------------------*/

void nim_sort (NIMAP *nim, SYMCMPFN cmpfn, void *data,
               int *map, int dir)
{                               /* --- sort name/identifier map */
  int i;                        /* loop variable */
  int **p;                      /* to traverse the value vector */

  assert(nim && cmpfn);         /* check the function arguments */
  v_sort(nim->ids, nim->cnt, cmpfn, data);
  if (!map) {                   /* if no conversion map is requested */
    for (p = nim->ids +(i = nim->cnt); --i >= 0; )
      **--p = i; }              /* just set new identifiers */
  else {                        /* if a conversion map is requested, */
    p = nim->ids +(i = nim->cnt);      /* traverse the sorted vector */
    if (dir < 0)                /* if backward map (i.e. new -> old) */
      while (--i >= 0) { map[i] = **--p; **p = i; }
    else                        /* if forward  map (i.e. old -> new) */
      while (--i >= 0) { map[**--p] = i; **p = i; }
  }                             /* (build conversion map) */
}  /* nim_sort() */

#endif
