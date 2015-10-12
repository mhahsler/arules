/*----------------------------------------------------------------------
  File    : tfscan.c
  Contents: table file scanner management
  Author  : Christian Borgelt
  History : 04.01.1998 file created
            11.03.1998 additional character flags enabled
            12.08.1998 function tfs_copy added
            01.09.1998 several assertions added
            27.09.1998 function tfs_getfld improved
            21.10.1998 bug in tfs_sgetc removed
            26.11.1998 some function parameters changed to const
            04.02.1999 long int changed to int
            16.11.1999 number of characters cleared for an empty field
            01.12.2000 '\r' made a default blank character
            14.07.2001 tfs_sgetc modified, tfs_buf and tfs_err added
            19.08.2001 last delimiter stored in TFSCAN structure
            11.02.2002 tfs_skip, tfs_reccnt, and tfs_reset added
----------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "tfscan.h"
#ifdef STORAGE
#include "storage.h"
#endif

/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
/* --- functions --- */
#define isblank(c)    tfs_istype(tfs, TFS_BLANK,  c)
#define isfldsep(c)   tfs_istype(tfs, TFS_FLDSEP, c)
#define isrecsep(c)   tfs_istype(tfs, TFS_RECSEP, c)
#define issep(c)      tfs_istype(tfs, TFS_FLDSEP|TFS_RECSEP, c)
#define iscomment(c)  tfs_istype(tfs, TFS_COMMENT, c)

/*----------------------------------------------------------------------
  Functions
----------------------------------------------------------------------*/

TFSCAN* tfs_create (void)
{                               /* --- create a table file scanner */
  TFSCAN *tfs;                  /* created table file scanner */
  int    i;                     /* loop variable */
  char   *p;                    /* to traverse character flags */

  tfs = (TFSCAN*)malloc(sizeof(TFSCAN));
  if (!tfs) return NULL;        /* allocate memory and */
  tfs->reccnt = 0;              /* initialize the fields */
  tfs->delim  = TFS_EOF;
  for (p = tfs->cflags +256, i = 256; --i >= 0; )
    *--p = '\0';                /* initialize the character flags */
  tfs->cflags['\n'] = TFS_RECSEP;
  tfs->cflags['\t'] = tfs->cflags[' '] = TFS_BLANK|TFS_FLDSEP;
  tfs->cflags['\r'] = TFS_BLANK;
  return tfs;                   /* return created table file scanner */
}  /* tfs_create() */

/*--------------------------------------------------------------------*/

TFSCAN* tfs_dup (const TFSCAN *tfs)
{                               /* --- duplicate a table file scanner */
  TFSCAN *dup;                  /* created duplicate */

  dup = (TFSCAN*)malloc(sizeof(TFSCAN));
  if (!dup) return NULL;        /* create a new table file scanner */
  tfs_copy(dup, tfs);           /* and copy source into it */
  return dup;                   /* return created duplicate */
}  /* tfs_dup() */

/*--------------------------------------------------------------------*/

void tfs_copy (TFSCAN *dst, const TFSCAN *src)
{                               /* --- copy a table file scanner */
  int  i;                       /* loop variable */
  char *d; const char *s;       /* to traverse the character flags */

  assert(src && dst);           /* check arguments */
  s = src->cflags +256; d = dst->cflags +256;
  for (i = 256; --i >= 0; ) *--d = *--s;
}  /* tfs_copy() */             /* copy character flags */

/*--------------------------------------------------------------------*/

int tfs_sgetc (TFSCAN *tfs, const char *s)
{                               /* --- get character from string */
  int c, code;                  /* character and character code */

  if (s) tfs->s = s;            /* if a new string is given, note it */
  if (*tfs->s == '\0')          /* if at the end of the old string, */
    return -1;                  /* abort the function */
  c = (unsigned char)*tfs->s++; /* get the next character */
  if (c != '\\')                /* if no quoted character, */
    return c;                   /* simply return the character */
  c = (unsigned char)*tfs->s++; /* get the next character */
  switch (c) {                  /* and evaluate it */
    case 'a': return '\a';      /* 0x07 (BEL) */
    case 'b': return '\b';      /* 0x08 (BS)  */
    case 'f': return '\f';      /* 0x0c (FF)  */
    case 'n': return '\n';      /* 0x0a (NL)  */
    case 'r': return '\r';      /* 0x0d (CR)  */
    case 't': return '\t';      /* 0x09 (HT)  */
    case 'v': return '\v';      /* 0x0b (VT)  */
    case '0': case '1': case '2': case '3':
    case '4': case '5': case '6': case '7':
      code = c -'0';            /* --- octal character code */
      c    = *tfs->s;           /* get the next character */
      if ((c >= '0') && (c <= '7')) code = (code << 3) +c -'0';
      else return code;         /* decode second digit */
      c    = *++tfs->s;         /* get the next character */
      if ((c >= '0') && (c <= '7')) code = (code << 3) +c -'0';
      else return c;            /* decode third digit */
      tfs->s++;                 /* consume the decoded character */
      return code & 0xff;       /* and return the character code */
    case 'x':                   /* --- hexadecimal character code */
      c = *tfs->s;              /* get the next character */
      if      ((c >= '0') && (c <= '9')) code = c -'0';
      else if ((c >= 'a') && (c <= 'f')) code = c -'a' +10;
      else if ((c >= 'A') && (c <= 'F')) code = c -'A' +10;
      else return 'x';          /* decode first digit */
      c = *++tfs->s;            /* get the next character */
      if      ((c >= '0') && (c <= '9')) code = (code << 4) +c -'0';
      else if ((c >= 'a') && (c <= 'f')) code = (code << 4) +c -'a' +10;
      else if ((c >= 'A') && (c <= 'F')) code = (code << 4) +c -'A' +10;
      else return code;         /* decode second digit */
      tfs->s++;                 /* consume the decoded character */
      return code;              /* and return the character code */
    default:                    /* non-function characters */
      if (*tfs->s == '\0') return '\\';
      else                 return (unsigned char)*tfs->s++;
  }                             /* return character or backslash */
}  /* tfs_sgetc() */

/*--------------------------------------------------------------------*/

int tfs_chars (TFSCAN *tfs, int type, const char *chars)
{                               /* --- set characters */
  int  i, c, d;                 /* loop variable, characters */
  char *p;                      /* to traverse character flags */

  assert(tfs);                  /* check argument */
  if (!chars) return -1;        /* if no characters given, abort */
  p = tfs->cflags +256;         /* clear character flags in type */
  for (i = 256; --i >= 0; ) *--p &= (char)~type;
  for (c = d = tfs_sgetc(tfs, chars); c >= 0; c = tfs_sgetc(tfs, NULL))
    tfs->cflags[c] |= (char)type;  /* set character flags */
  return (d >= 0) ? d : 0;      /* return first character */
}  /* tfs_chars() */

/*--------------------------------------------------------------------*/

int tfs_getfld (TFSCAN *tfs, FILE *file, char *buf, int len)
{                               /* --- read a table field */
  int  c;                       /* character read */
  int  d;                       /* delimiter type */
  char *p;                      /* to traverse the buffer */

  assert(tfs && file && (!buf || (len >= 0)));
  if (!buf) {                   /* if no buffer given, use internal */
    buf = tfs->buf; len = TFS_SIZE; }
  p = buf; *p = '\0';           /* clear the read buffer and */
  tfs->cnt = 0;                 /* the number of characters read */
  do {                          /* --- skip leading blanks */
    c = getc(file);             /* get the next character */
    if (c == EOF) return tfs->delim = (ferror(file)) ? -1 : TFS_EOF;
  } while (isblank(c));         /* while the character is blank */
  if (issep(c)) {               /* check for field/record separator */
    if (isfldsep(c)) return tfs->delim = TFS_FLD;
    tfs->reccnt++;   return tfs->delim = TFS_REC;
  }                             /* if at end of record, count reocrd */
  while (1) {                   /* --- read value */
    if (len > 0) {              /* if the buffer is not full, */
      len--; *p++ = (char)c; }  /* store the character in the buffer */
    c = getc(file);             /* get the next character */
    if (issep(c)) { d = (isfldsep(c))  ? TFS_FLD : TFS_REC; break; }
    if (c == EOF) { d = (ferror(file)) ? -1      : TFS_EOF; break; }
  }                             /* while character is no separator */
  while (isblank(*--p));        /* --- remove trailing blanks */
  *++p = '\0';                  /* terminate string in buffer */
  tfs->cnt = (int)(p -buf);     /* store number of characters read */
  if (d != TFS_FLD) {           /* if not at a field separator */
    if (d == TFS_REC) tfs->reccnt++;
    return tfs->delim = d;      /* if at end of record, count record, */
  }                             /* and then abort the function */
  while (isblank(c)) {          /* --- skip trailing blanks */
    c = getc(file);             /* get the next character */
    if (c == EOF) return tfs->delim = ferror(file) ? -1 : TFS_EOF;
  }                             /* check for end of file */
  if (isrecsep(c)) {            /* check for a record separator */
    tfs->reccnt++; return tfs->delim = TFS_REC; }
  if (!isfldsep(c))             /* put back character (may be */
    ungetc(c, file);            /* necessary if blank = field sep.) */
  return tfs->delim = TFS_FLD;  /* return the delimiter type */
}  /* tfs_getfld() */

/*--------------------------------------------------------------------*/

int tfs_skip (TFSCAN *tfs, FILE *file)
{                               /* --- skip comment records */
  int c;                        /* character read */

  assert(tfs && file);          /* check the function arguments */
  while (1) {                   /* comment read loop */
    c = getc(file);             /* read the next character */
    if (c == EOF) return tfs->delim = ferror(file) ? -1 : TFS_EOF;
    if (!iscomment(c)) {        /* if the next char. is no comment, */
      ungetc(c, file); return 0; }         /* put it back and abort */
    while (!isrecsep(c)) {      /* while not at end of record */
      c = fgetc(file);          /* get and check the next character */
      if (c == EOF) return tfs->delim = ferror(file) ? -1 : TFS_EOF;
    }                           /* consume/skip all characters */
    tfs->reccnt++;              /* up to the end of the record */
  }                             /* and count the record read */
  return tfs->delim = TFS_REC;  /* return the delimiter type */
}  /* tfs_skip() */
