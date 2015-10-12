/*----------------------------------------------------------------------
  File    : tfscan.h
  Contents: table file scanner management
  Author  : Christian Borgelt
  History : 04.01.1998 file created
            11.03.1998 additional character flags enabled
            12.08.1998 function tfs_copy added
            26.11.1998 some function parameters changed to const
            29.11.1998 function tfs_dup added
            04.02.1999 long int changed to int
            14.07.2001 tfs_sgetc modified, tfs_buf and tfs_err added
            19.08.2001 tfs_delim added (last delimiter type)
            11.02.2002 tfs_skip, tfs_reccnt, and tfs_reset added
----------------------------------------------------------------------*/
#ifndef __TFSCAN__
#define __TFSCAN__
#include <stdio.h>

/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
/* --- character flags --- */
#define TFS_RECSEP   0x01       /* flag for record separator */
#define TFS_FLDSEP   0x02       /* flag for field separator */
#define TFS_BLANK    0x04       /* flag for blank character */
#define TFS_COMMENT  0x08       /* flag for comment character */
#define TFS_OTHER    0x10       /* flag for other character type */

/* --- delimiter types --- */
#define TFS_EOF      0          /* end of file delimiter */
#define TFS_REC      1          /* record delimiter */
#define TFS_FLD      2          /* field  delimiter */

/* --- buffer size --- */
#define TFS_SIZE   256          /* size of internal read buffer */

/*----------------------------------------------------------------------
  Type Definitions
----------------------------------------------------------------------*/
typedef struct {                /* --- error information --- */
  int        code;              /* error code */
  int        rec, fld;          /* record and field number */
  int        exp;               /* expected number of records/fields */
  char       *s;                /* a string (e.g., field contents) */
} TFSERR;                       /* (error information) */

typedef struct {                /* --- table file scanner --- */
  char       cflags[256];       /* character flags */
  const char *s;                /* string pointer for tfs_sgetc */
  int        reccnt;            /* number of records read */
  int        delim;             /* last delimiter read */
  int        cnt;               /* number of characters read */
  char       buf[TFS_SIZE+4];   /* read buffer */
  TFSERR     err;               /* error information */
} TFSCAN;                       /* (table file scanner) */

/*----------------------------------------------------------------------
  Functions
----------------------------------------------------------------------*/
extern TFSCAN* tfs_create (void);
extern void    tfs_delete (TFSCAN *tfs);
extern TFSCAN* tfs_dup    (const TFSCAN *tfs);
extern void    tfs_copy   (TFSCAN *dst, const TFSCAN *src);

extern int     tfs_sgetc  (TFSCAN *tfs, const char *s);
extern int     tfs_chars  (TFSCAN *tfs, int type, const char *chars);
extern int     tfs_istype (const TFSCAN *tfs, int type, int c);

extern int     tfs_getfld (TFSCAN *tfs, FILE *file, char *buf, int len);
extern int     tfs_delim  (TFSCAN *tfs);
extern int     tfs_cnt    (TFSCAN *tfs);
extern char*   tfs_buf    (TFSCAN *tfs);

extern int     tfs_skip   (TFSCAN *tfs, FILE *file);
extern int     tfs_reccnt (TFSCAN *tfs);
extern void    tfs_reset  (TFSCAN *tfs);

extern TFSERR* tfs_err    (TFSCAN *tfs);

/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
#define tfs_delete(s)     free(s)

#define tfs_istype(s,t,c) ((s)->cflags[(unsigned char)(c)] & (t))

#define tfs_delim(s)      ((s)->delim)
#define tfs_cnt(s)        ((s)->cnt)
#define tfs_buf(s)        ((s)->buf)

#define tfs_reccnt(s)     ((s)->reccnt)
#define tfs_reset(s)      ((s)->reccnt = 0)

#define tfs_err(s)        (&(s)->err)
#endif
