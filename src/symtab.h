/*----------------------------------------------------------------------
  File    : symtab.h
  Contents: symbol table and name/identifier map management
  Author  : Christian Borgelt
  History : 22.10.1995 file created
            30.10.1995 functions made independent of symbol data
            26.11.1995 symbol types and visibility levels added
            04.01.1996 st_clear added
            27.02.1996 st_insert modified, st_name and st_type added
            26.03.1996 insertion into hash bucket simplified
            28.06.1996 dynamic bucket vector enlargement added
            01.04.1997 functions st_clear and st_remove combined
            31.05.1998 list of all symbols removed
            20.06.1998 deletion function moved to st_create
            28.09.1998 types ULONG and CCHAR removed, st_stats added
            04.02.1999 long int changed to int
            10.11.1999 name/identifier map management added
----------------------------------------------------------------------*/
#ifndef __SYMTAB__
#define __SYMTAB__

/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
#define EXISTS  ((void*)-1)     /* symbol exists already */
#define NIMAP   SYMTAB          /* name/id maps are special sym.tabs. */

/*----------------------------------------------------------------------
  Type Definitions
----------------------------------------------------------------------*/
typedef unsigned HASHFN   (const char *name, int type);
typedef void     SYMFN    (void *data);
typedef int      SYMCMPFN (const void *s1, const void *s2, void *data);

typedef struct _ste {           /* --- symbol table element --- */
  struct _ste *succ;            /* successor in hash bucket */
  char        *name;            /* symbol name */
  int         type;             /* symbol type */
  int         level;            /* visibility level */
} STE;                          /* (symbol table element) */

typedef struct {                /* --- symbol table --- */
  int         cnt;              /* current number of symbols */
  int         level;            /* current visibility level */
  int         size;             /* current hash table size */
  int         max;              /* maximal hash table size */
  HASHFN      *hash;            /* hash function */
  SYMFN       *delfn;           /* symbol deletion function */
  STE         **bvec;           /* bucket vector */
  int         vsz;              /* size of identifier vector */
  int         **ids;            /* identifier vector */
} SYMTAB;                       /* (symbol table) */

/*----------------------------------------------------------------------
  Symbol Table Functions
----------------------------------------------------------------------*/
extern SYMTAB*     st_create  (int init, int max,
                               HASHFN hash, SYMFN delfn);
extern void        st_delete  (SYMTAB *tab);
extern void*       st_insert  (SYMTAB *tab, const char *name, int type,
                               unsigned size);
extern int         st_remove  (SYMTAB *tab, const char *name, int type);
extern void*       st_lookup  (SYMTAB *tab, const char *name, int type);
extern void        st_begblk  (SYMTAB *tab);
extern void        st_endblk  (SYMTAB *tab);
extern int         st_symcnt  (const SYMTAB *tab);
extern const char* st_name    (const void *data);
extern int         st_type    (const void *data);
#ifndef NDEBUG
extern void        st_stats   (const SYMTAB *tab);
#endif

/*----------------------------------------------------------------------
  Name/Identifier Map Functions
----------------------------------------------------------------------*/
#ifdef NIMAPFN
extern NIMAP*      nim_create (int init, int max,
                               HASHFN hash, SYMFN delfn);
extern void        nim_delete (NIMAP *nim);
extern void*       nim_add    (NIMAP *nim, const char *name,
                               unsigned size);
extern void*       nim_byname (NIMAP *nim, const char *name);
extern void*       nim_byid   (NIMAP *nim, int id);
extern const char* nim_name   (const void *data);
extern int         nim_cnt    (const NIMAP *nim);
extern void        nim_sort   (NIMAP *nim, SYMCMPFN cmpfn, void *data,
                               int *map, int dir);
#ifndef NDEBUG
extern void        nim_stats  (const NIMAP *nimap);
#endif
#endif
/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
#define st_begblk(t)      ((t)->level++)
#define st_symcnt(t)      ((t)->cnt)
#define st_name(d)        ((const char*)((STE*)(d)-1)->name)
#define st_type(d)        (((STE*)(d)-1)->type)

/*--------------------------------------------------------------------*/
#ifdef NIMAPFN
#define nim_delete(m)     st_delete(m)
#define nim_add(m,n,s)    st_insert(m,n,0,s)
#define nim_byname(m,n)   st_lookup(m,n,0)
#define nim_byid(m,i)     ((void*)(m)->ids[i])
#define nim_name(d)       st_name(d)
#define nim_cnt(m)        st_symcnt(m)
#ifndef NDEBUG
#define nim_stats(m)      st_stats(m)
#endif
#endif
#endif
