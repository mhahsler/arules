/*----------------------------------------------------------------------
  File    : scan.c
  Contents: scanner (lexical analysis of a character stream)
  Author  : Christian Borgelt
  History : 16.01.1996 file created
            21.02.1996 identifier recognition made more flexible
            17.03.1996 keyword tokens removed
            15.04.1996 duplicate state removed from sc_next
            29.07.1997 < and > declared active (for decision trees)
            08.09.1997 escape sequences in strings made possible
            11.09.1997 single characters stored also in scn->value
            08.02.1998 recover and error message functions added
            09.02.1998 bug in state S_NUMPT concerning "-." removed
            13.02.1998 token T_RGT ('->') added
            04.03.1998 returned tokens changed for some states
            17.04.1998 token T_LFT ('<-') added
            27.05.1998 token T_CMP (two char comparison operator) added
            31.05.1998 token conversion to number removed
            08.02.1999 reading from standard input made possible
            29.04.1999 quoted string parsing improved
            13.11.1999 token string length stored in scn->len
            23.11.2000 functions sc_fmtlen and sc_format added
            15.07.2001 scanner made an object, state definitions added
            16.07.2001 characters with code > 127 made printable
                       look ahead functionality added (sc_back)
----------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include "scan.h"
#ifdef STORAGE
#include "storage.h"
#endif

/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
#ifdef SC_SCAN
/* --- character classes --- */
#define C_ILLEGAL    0          /* illegal character */
#define C_SPACE      1          /* white space, e.g. ' ' '\t' '\n' */
#define C_LETTER     2          /* letter or underscore '_' */
#define C_DIGIT      3          /* digit */
#define C_POINT      4          /* point, '.' */
#define C_SIGN       5          /* sign,  '+' or '-' */
#define C_SLASH      6          /* slash, '/' */
#define C_QUOTE      7          /* quote, e.g. '"' '`' */
#define C_CMPOP      8          /* comparison operator, e.g. '<' */
#define C_ACTIVE     9          /* active characters, e.g. ',' '(' */

/* --- scanner states --- */
#define S_SPACE      0          /* skipping white space */
#define S_ID         1          /* reading identifier */
#define S_NUMDIG     2          /* reading number, digit */
#define S_NUMPT      3          /* reading number, decimal point */
#define S_FRAC       4          /* reading number, digit and point */
#define S_EXPIND     5          /* reading exponent, indicator */
#define S_EXPSGN     6          /* reading exponent, sign */
#define S_EXPDIG     7          /* reading exponent, digit */
#define S_SIGN       8          /* sign read */
#define S_CMPOP      9          /* reading comparison operator */
#define S_STRING    10          /* reading quoted string */
#define S_ESC       11          /* reading escaped character */
#define S_OCT1      12          /* reading octal  number, 1 digit */
#define S_OCT2      13          /* reading octal  number, 2 digits */
#define S_HEX1      14          /* reading hexad. number, 1 digit */
#define S_HEX2      15          /* reading hexad. number, 2 digits */
#define S_SLASH     16          /* slash read */
#define S_CPPCOM    17          /* reading C++ comment */
#define S_CCOM1     18          /* reading C comment */
#define S_CCOM2     19          /* reading C comment, possible end */
#define S_CCOM3     20          /* reading C comment, possible start */

/* --- functions --- */
#define UNGETC(s,c)  do { if ((c) ==  EOF) break; \
                          if ((c) == '\n') (s)->line--; \
                          ungetc(c, (s)->file); } while (0)

/* --- additional error codes --- */
#define E_UNKNOWN   (-11)       /* unknown error */
#define MSGOFFSET   (-16)       /* offset for add. error messages */

/* --- texts --- */
#ifdef GERMAN                   /* deutsche Texte */
#define FILETXT     "Datei"
#define LINETXT     "Zeile"
#else                           /* English texts */
#define FILETXT     "file"
#define LINETXT     "line"
#endif  /* #ifdef GERMAN .. #else .. */
#endif  /* #ifdef SC_SCAN */

/*----------------------------------------------------------------------
  Constants
----------------------------------------------------------------------*/
static const char _scftab[256] = {  /* scanable form classes */
        /* NUL  SOH  STX  ETX  EOT  ENQ  ACK  BEL */
/* 00 */    2,   2,   2,   2,   2,   2,   2,  'a',
        /*  BS   HT   LF   VT   FF   CR   SO   SI */
           'b', 't', 'n', 'v', 'f', 'r',  2,   2,
        /* DLE  DC1  DC2  DC3  DC4  NAK  SYN  ETB */
/* 10 */    2,   2,   2,   2,   2,   2,   2,   2,
        /* CAN   EM  SUB  ESC   FS   GS   RS   US */
            2,   2,   2,   2,   2,   2,   2,   2,
        /* ' '  '!'  '"'  '#'  '$'  '%'  '&'  ''' */
/* 20 */    1,   1,  '"',  1,   1,   1,   1,   1,
        /* '('  ')'  '*'  '+'  ','  '-'  '.'  '/' */
            1,   1,   1,   0,   1,   0,   0,   1,
        /* '0'  '1'  '2'  '3'  '4'  '5'  '6'  '7' */
/* 30 */    0,   0,   0,   0,   0,   0,   0,   0,
        /* '8'  '9'  ':'  ';'  '<'  '='  '>'  '?' */
            0,   0,   1,   1,   1,   1,   1,   1,
        /* '@'  'A'  'B'  'C'  'D'  'E'  'F'  'G' */
/* 40 */    1,   0,   0,   0,   0,   0,   0,   0,
        /* 'H'  'I'  'J'  'K'  'L'  'M'  'N'  'O' */
            0,   0,   0,   0,   0,   0,   0,   0,
        /* 'P'  'Q'  'R'  'S'  'T'  'U'  'V'  'W' */
/* 50 */    0,   0,   0,   0,   0,   0,   0,   0,
        /* 'X'  'Y'  'Z'  '['  '\'  ']'  '^'  '_' */
            0,   0,   0,   1, '\\',  1,   1,   0,
        /* '`'  'a'  'b'  'c'  'd'  'e'  'f'  'g' */
/* 60 */    1,   0,   0,   0,   0,   0,   0,   0,
        /* 'h'  'i'  'j'  'k'  'l'  'm'  'n'  'o' */
            0,   0,   0,   0,   0,   0,   0,   0,
        /* 'p'  'q'  'r'  's'  't'  'u'  'v'  'w' */
/* 70 */    0,   0,   0,   0,   0,   0,   0,   0,
        /* 'x'  'y'  'z'  '{'  '|'  '}'  '~'  DEL */
            0,   0,   0,   1,   1,   1,   1,   2,
/* 80 */    1,   1,   1,   1,   1,   1,   1,   1,
            1,   1,   1,   1,   1,   1,   1,   1,
/* 90 */    1,   1,   1,   1,   1,   1,   1,   1,
            1,   1,   1,   1,   1,   1,   1,   1,
/* a0 */    1,   1,   1,   1,   1,   1,   1,   1,
            1,   1,   1,   1,   1,   1,   1,   1,
/* b0 */    1,   1,   1,   1,   1,   1,   1,   1,
            1,   1,   1,   1,   1,   1,   1,   1,
/* c0 */    1,   1,   1,   1,   1,   1,   1,   1,
            1,   1,   1,   1,   1,   1,   1,   1,
/* d0 */    1,   1,   1,   1,   1,   1,   1,   1,
            1,   1,   1,   1,   1,   1,   1,   1,
/* e0 */    1,   1,   1,   1,   1,   1,   1,   1,
            1,   1,   1,   1,   1,   1,   1,   1,
/* f0 */    1,   1,   1,   1,   1,   1,   1,   1,
            1,   1,   1,   1,   1,   1,   1,   1 };

#ifdef SC_SCAN
static const char _ccltab[256] = {  /* character classes */
        /* NUL  SOH  STX  ETX  EOT  ENQ  ACK  BEL */
/* 00 */    0,   0,   0,   0,   0,   0,   0,   0,
        /*  BS   HT   LF   VT   FF   CR   SO   SI */
            0,   1,   1,   1,   1,   1,   0,   0,
        /* DLE  DC1  DC2  DC3  DC4  NAK  SYN  ETB */
/* 10 */    0,   0,   0,   0,   0,   0,   0,   0,
        /* CAN   EM  SUB  ESC   FS   GS   RS   US */
            0,   0,   0,   0,   0,   0,   0,   0,
        /* ' '  '!'  '"'  '#'  '$'  '%'  '&'  ''' */
/* 20 */    1,   8,   7,   9,   9,   9,   9,   7,
        /* '('  ')'  '*'  '+'  ','  '-'  '.'  '/' */
            9,   9,   9,   5,   9,   5,   4,   6,
        /* '0'  '1'  '2'  '3'  '4'  '5'  '6'  '7' */
/* 30 */    3,   3,   3,   3,   3,   3,   3,   3,
        /* '8'  '9'  ':'  ';'  '<'  '='  '>'  '?' */
            3,   3,   9,   9,   8,   8,   8,   9,
        /* '@'  'A'  'B'  'C'  'D'  'E'  'F'  'G' */
/* 40 */    0,   2,   2,   2,   2,   2,   2,   2,
        /* 'H'  'I'  'J'  'K'  'L'  'M'  'N'  'O' */
            2,   2,   2,   2,   2,   2,   2,   2,
        /* 'P'  'Q'  'R'  'S'  'T'  'U'  'V'  'W' */
/* 50 */    2,   2,   2,   2,   2,   2,   2,   2,
        /* 'X'  'Y'  'Z'  '['  '\'  ']'  '^'  '_' */
            2,   2,   2,   9,   9,   9,   9,   2,
        /* '`'  'a'  'b'  'c'  'd'  'e'  'f'  'g' */
/* 60 */    7,   2,   2,   2,   2,   2,   2,   2,
        /* 'h'  'i'  'j'  'k'  'l'  'm'  'n'  'o' */
            2,   2,   2,   2,   2,   2,   2,   2,
        /* 'p'  'q'  'r'  's'  't'  'u'  'v'  'w' */
/* 70 */    2,   2,   2,   2,   2,   2,   2,   2,
        /* 'x'  'y'  'z'  '{'  '|'  '}'  '~'  DEL */
            2,   2,   2,   9,   9,   9,   9,   0,
/* 80 */    0,   0,   0,   0,   0,   0,   0,   0,
            0,   0,   0,   0,   0,   0,   0,   0,
/* 90 */    0,   0,   0,   0,   0,   0,   0,   0,
            0,   0,   0,   0,   0,   0,   0,   0,
/* a0 */    0,   0,   0,   0,   0,   0,   0,   0,
            0,   0,   0,   0,   0,   0,   0,   0,
/* b0 */    0,   0,   0,   0,   0,   0,   0,   0,
            0,   0,   0,   0,   0,   0,   0,   0,
/* c0 */    0,   0,   0,   0,   0,   0,   0,   0,
            0,   0,   0,   0,   0,   0,   0,   0,
/* d0 */    0,   0,   0,   0,   0,   0,   0,   0,
            0,   0,   0,   0,   0,   0,   0,   0,
/* e0 */    0,   0,   0,   0,   0,   0,   0,   0,
            0,   0,   0,   0,   0,   0,   0,   0,
/* f0 */    0,   0,   0,   0,   0,   0,   0,   0,
            0,   0,   0,   0,   0,   0,   0,   0 };

#ifdef GERMAN                      /* deutsche Texte */
static const char *_errmsgs[] = {  /* error messages */
  /* E_NONE      0 */  "kein Fehler",
  /* E_NOMEM    -1 */  "nicht genug Speicher",
  /* E_FOPEN    -2 */  "Öffnen fehlgeschlagen",
  /* E_FREAD    -3 */  "Lesefehler",
  /* E_FWRITE   -4 */  "Schreibfehler",
  /* E_ILLCHR   -5 */  "ungültiges Zeichen '%c' (0x%02x)",
  /* E_BUFOVF   -6 */  "Pufferüberlauf",
  /* E_UNTSTR   -7 */  "unbeendete Zeichenkette",
  /* E_UNTCOM   -8 */  "unerwartetes Dateiende in Kommentar "
                       "(Anfang in Zeile %d)",
  /* E_STATE    -9 */  "ungültiger Scannerzustand",
  /* E_GARBAGE -10 */  "ungültiger Text am Dateiende",
  /* E_UNKNOWN -11 */  "unbekannter Fehler"
};
#else                              /* English texts */
static const char *_errmsgs[] = {  /* error messages */
  /* E_NONE      0 */  "no error",
  /* E_NOMEM    -1 */  "not enough memory",
  /* E_FOPEN    -2 */  "file open failed",
  /* E_FREAD    -3 */  "file read failed",
  /* E_FWRITE   -4 */  "file write failed",
  /* E_ILLCHR   -5 */  "illegal character '%c' (0x%02x)",
  /* E_BUFOVF   -6 */  "scan buffer overflow",
  /* E_UNTSTR   -7 */  "unterminated string",
  /* E_UNTCOM   -8 */  "unexpected end of file in comment "
                       "started on line %d",
  /* E_STATE    -9 */  "illegal scanner state",
  /* E_GARBAGE -10 */  "garbage at end of file",
  /* E_UNKNOWN -11 */  "unknown error"
};
#endif  /* #ifdef GERMAN .. #else .. */
#endif  /* #ifdef SC_SCAN */

/*----------------------------------------------------------------------
  Auxiliary Functions
----------------------------------------------------------------------*/
#ifdef SC_SCAN

static int _swap (SCAN *scan)
{                               /* --- swap token information */
  int t;                        /* swap buffer */

  if (scan->value == scan->buf[0]) scan->value = scan->buf[1];
  else                             scan->value = scan->buf[0];
  t = scan->plen;   scan->plen   = scan->len;   scan->len   = t;
  t = scan->pline;  scan->pline  = scan->line;  scan->line  = t;
  t = scan->ptoken; scan->ptoken = scan->token; scan->token = t;
  return t;                     /* return the new token */
}  /* _swap() */

#endif
/*----------------------------------------------------------------------
  Main Functions
----------------------------------------------------------------------*/

int sc_fmtlen (const char *s, int *len)
{                               /* --- length of a formatted name */
  int n = 0, k = 0;             /* number of (additional) characters */
  int q = 0;                    /* quote flag (default: no quotes) */

  assert(s);                    /* check the function arguments */
  while (*s) {                  /* while not at end of name */
    n++;                        /* count character */
    switch (_scftab[(unsigned char)*s++]) {
      case  0:                break;
      case  1:         q = 2; break;
      case  2: k += 3; q = 2; break;
      default: k += 1; q = 2; break;
    }                           /* sum additional characters and */
  }                             /* set quote flag (if necessary) */
  if (len) *len = n;            /* store normal length and */
  return n +k +q;               /* return length of scanable form */
}  /* sc_fmtlen() */

/*--------------------------------------------------------------------*/

int sc_format (char *dst, const char *src, int quotes)
{                               /* --- format name in scanable form */
  char *d; const char *s;       /* to traverse buffer and name */
  int  c, cls;                  /* character and character class */
  int  t;                       /* temporary buffer */

  assert(dst && src);           /* check the function arguments */
  if (!*src) quotes = 1;        /* an empty name needs quotes */
  if (!quotes) {                /* if quotes are not mandatory, */
    for (s = src; *s; )         /* traverse the string to convert */
      if (_scftab[(unsigned char)*s++] != 0) {
        quotes = 1; break; }    /* if a character needs quotes, */
  }                             /* set the quotes flag and abort */
  d = dst;                      /* get the destination and */
  if (quotes) *d++ = '"';       /* store a quote if necessary */
  while (*src) {                /* traverse the characters */
    c   = (unsigned char)*src++;/* get the next character */
    cls = _scftab[c];           /* and its character class */
    if      (cls < 2)           /* if it is a normal character, */
      *d++ = c;                 /* just store it */
    else if (cls > 2) {         /* if it is an ANSI escape character, */
      *d++ = '\\'; *d++ = cls;} /* store it as '\c' */
    else {                      /* if it is any other character */
      *d++ = '\\'; *d++ = 'x';
      t = c >> 4;  *d++ = (t > 9) ? (t -10 +'a') : (t +'0');
      t = c & 0xf; *d++ = (t > 9) ? (t -10 +'a') : (t +'0');
    }                           /* store the character code */
  }                             /* as a hexadecimal number */
  if (quotes) *d++ = '"';       /* store the closing quote */
  *d = '\0';                    /* and terminate the string */
  return (int)(d -dst);         /* return the length of the result */
}  /* sc_format() */

/*--------------------------------------------------------------------*/
#ifdef SC_SCAN

SCAN* sc_create (const char *fname)
{                               /* --- create a scanner */
  const char *fn = fname;       /* buffer for filename */
  SCAN       *scan;             /* created scanner */

  if (!fn || !*fn) fname = "<stdin>";
  scan = (SCAN*)malloc(sizeof(SCAN) +strlen(fname));
  if (!scan) return NULL;       /* allocate memory for a scanner */
  strcpy(scan->fname, fname);   /* and note the file name */
  if (!fn || !*fn)              /* if no file name is given, */
    scan->file = stdin;         /* read from standard input */
  else {                        /* if a  file name is given, */
    scan->file = fopen(fn,"r"); /* open the file for reading */
    if (!scan->file) { free(scan); return NULL; }
  }
  scan->line    = 1;            /* initialize the fields */
  scan->token   = scan->len   = scan->start = 0;
  scan->value   = scan->buf[0]; scan->buf[0][0] = '\0';
  scan->back    = 0;
  scan->errfile = stderr;
  scan->msgcnt  = scan->lncnt = 0;
  scan->msgs    = NULL;
  return scan;                  /* return created scanner */
}  /* sc_create() */

/*--------------------------------------------------------------------*/

void sc_delete (SCAN *scan)
{                               /* --- delete a scanner */
  if (scan->file != stdin) fclose(scan->file);
  free(scan);                   /* close the input file and */
}  /* sc_delete() */            /* delete the scanner structure */

/*--------------------------------------------------------------------*/

int sc_next (SCAN *scan)
{                               /* --- get next token */
  int  c, ccl;                  /* character and character class */
  int  quote = 0;               /* quote at the start of a string */
  int  ec    = 0;               /* escaped character */
  int  state = 0;               /* state of automaton */
  int  level = 0;               /* comment nesting level */
  char *p;                      /* to traverse the scan buffer */
  char *end;                    /* end of the scan buffer */

  if (scan->back) {             /* if a step backwards has been made, */
    scan->back = 0;             /* clear the corresponding flag, */
    return _swap(scan);         /* swap back the token information, */
  }                             /* and return the current token */
  scan->pline  = scan->line;    /* note the relevant information */
  scan->ptoken = scan->token;   /* of the current token */
  scan->plen   = scan->len;     /* and swap scan buffers */
  if (scan->value == scan->buf[0]) scan->value = p = scan->buf[1];
  else                             scan->value = p = scan->buf[0];
  end = p +SC_BUFSIZE -1;       /* get the end of the scan buffer */

  while (1) {                   /* read loop */
    c   = getc(scan->file);     /* get character and character class */
    ccl = (c < 0) ? EOF : _ccltab[c];
    if (c == '\n') scan->line++; /* count the line */

    switch (state) {            /* evaluate state of automaton */

      case S_SPACE:             /* --- skip white space */
        switch (ccl) {          /* evaluate character category */
          case C_SPACE : /* do nothing */             break;
          case C_LETTER: *p++  = c; state = S_ID;     break;
          case C_DIGIT : *p++  = c; state = S_NUMDIG; break;
          case C_POINT : *p++  = c; state = S_NUMPT;  break;
          case C_SIGN  : *p++  = c; state = S_SIGN;   break;
          case C_CMPOP : *p++  = c; state = S_CMPOP;  break;
          case C_QUOTE : quote = c; state = S_STRING; break;
          case C_SLASH :            state = S_SLASH;  break;
          case C_ACTIVE: *p++  = c; *p = '\0'; scan->len = 1;
                         return scan->token = c;
          case EOF     : strcpy(p, "<eof>");   scan->len = 4;
                         return scan->token = (ferror(scan->file))
                                           ? E_FREAD : T_EOF;
          default      : *p++  = c; *p = '\0'; scan->len = 1;
                         return scan->token = E_ILLCHR;
        } break;

      case S_ID:                /* --- identifier (letter read) */
        if ((ccl == C_LETTER)   /* if another letter */
        ||  (ccl == C_DIGIT)    /* or a digit */
        ||  (ccl == C_POINT)    /* or a decimal point */
        ||  (ccl == C_SIGN)) {  /* or a sign follows */
          if (p >= end) return scan->token = E_BUFOVF;
          *p++ = c; break;      /* buffer character */
        }                       /* otherwise */
        UNGETC(scan, c);        /* put back last character, */
        *p = '\0';              /* terminate string in buffer */
        scan->len = (int)(p -scan->value); /* set string length */
        return scan->token = T_ID;   /* and return 'identifier' */

      case S_NUMDIG:            /* --- number (digit read) */
        if (p < end) *p++ = c;  /* buffer character */
        else return scan->token = E_BUFOVF;
        if  (ccl == C_DIGIT)    /* if another digit follows, */
          break;                /* do nothing */
        if  (ccl == C_POINT) {  /* if a decimal point follows, */
          state = S_FRAC;   break; } /* go to 'fraction' state */
        if ((c == 'e')          /* if an exponent indicator follows */
        ||  (c == 'E')) {       /* (lower- or uppercase), */
          state = S_EXPIND; break; } /* go to 'exponent' state */
        if ((ccl == C_LETTER)   /* if a letter */
        ||  (ccl == C_SIGN)) {  /* or a sign follows, */
          state = S_ID; break;  /* go to 'identifier' state */
        }                       /* otherwise */
        UNGETC(scan, c);        /* put back last character, */
        *--p = '\0';            /* terminate string in buffer */
        scan->len = (int)(p -scan->value); /* set string length */
        return scan->token = T_NUM;      /* and return 'number' */

      case S_NUMPT:             /* --- number (point read) */
        if (p < end) *p++ = c;  /* buffer character */
        else return scan->token = E_BUFOVF;
        if  (ccl == C_DIGIT) {       /* if a digit follows, */
          state = S_FRAC; break; }   /* go to 'fraction' state */
        if ((ccl == C_LETTER)   /* if a letter */
        ||  (ccl == C_POINT)    /* or a decimal point */
        ||  (ccl == C_SIGN)) {  /* or a sign follows */
          state = S_ID; break;  /* go to 'identifier' state */
        }                       /* otherwise */
        UNGETC(scan, c);        /* put back last character, */
        *--p = '\0';            /* terminate string in buffer */
        scan->len = (int)(p -scan->value); /* set string length */
        return scan->token = T_ID;   /* and return 'identifier' */

      case S_FRAC:              /* --- number (digit & point read) */
        if (p < end) *p++ = c;  /* buffer character */
        else return scan->token = E_BUFOVF;
        if  (ccl == C_DIGIT)    /* if another digit follows, */
          break;                /* do nothing else */
        if ((c == 'e')          /* if an exponent indicator follows, */
        ||  (c == 'E')) {       /* (lower- or uppercase), */
          state = S_EXPIND; break; } /* go to exponent state */
        if ((ccl == C_LETTER)   /* if a letter */
        ||  (ccl == C_POINT)    /* or a decimal point */
        ||  (ccl == C_SIGN)) {  /* or a sign follows, */
          state = S_ID; break;  /* go to 'identifier' state */
        }                       /* otherwise */
        UNGETC(scan, c);        /* put back last character, */
        *--p = '\0';            /* terminate string in buffer */
        scan->len = (int)(p -scan->value); /* set string length */
        return scan->token = T_NUM;      /* and return 'number' */

      case S_EXPIND:            /* --- exponent (indicator read) */
        if (p < end) *p++ = c;  /* buffer character */
        else return scan->token = E_BUFOVF;
        if  (ccl == C_SIGN) {        /* if a sign follows, */
          state = S_EXPSGN; break; } /* go to 2nd 'exponent' state */
        if  (ccl == C_DIGIT) {       /* if a digit follows, */
          state = S_EXPDIG; break; } /* go to 3rd 'exponent' state */
        if ((ccl == C_LETTER)   /* if a letter */
        ||  (ccl == C_POINT)) { /* or a decimal point follows */
          state = S_ID; break;  /* go to 'identifier' state */
        }                       /* otherwise */
        UNGETC(scan, c);        /* put back last character, */
        *--p = '\0';            /* terminate string in buffer */
        scan->len = (int)(p -scan->value); /* set string length */
        return scan->token = T_ID;   /* and return 'identifier' */

      case S_EXPSGN:            /* --- exponent (sign read) */
        if (p < end) *p++ = c;  /* buffer character */
        else return scan->token = E_BUFOVF;
        if  (ccl == C_DIGIT) {      /* if a digit follows, */
          state = S_EXPDIG; break;} /* do nothing else */
        if ((ccl == C_LETTER)   /* if a letter */
        ||  (ccl == C_POINT)    /* or a decimal point */
        ||  (ccl == C_SIGN)) {  /* or a sign follows */
          state = S_ID; break;  /* go to 'identifier' state */
        }                       /* otherwise */
        UNGETC(scan, c);        /* put back last character, */
        *--p = '\0';            /* terminate string in buffer */
        scan->len = (int)(p -scan->value); /* set string length */
        return scan->token = T_ID;   /* and return 'identifier' */

      case S_EXPDIG:            /* --- exponent (digit read) */
        if (p < end) *p++ = c;  /* buffer character */
        else return scan->token = E_BUFOVF;
        if  (ccl == C_DIGIT)    /* if another digit follows, */
          break;                /* do nothing else */
        if ((ccl == C_LETTER)   /* if a letter */
        ||  (ccl == C_POINT)    /* or a decimal point */
        ||  (ccl == C_SIGN)) {  /* or a sign follows, */
          state = S_ID; break;  /* go to 'identifier' state */
        }                       /* otherwise */
        UNGETC(scan, c);        /* put back last character, */
        *--p = '\0';            /* terminate string in buffer */
        scan->len = (int)(p -scan->value); /* set string length */
        return scan->token = T_NUM;      /* and return 'number' */

      case S_SIGN:              /* --- number (sign read) */
        *p++ = c;               /* buffer character */
        if  (ccl == C_DIGIT) {       /* if a digit follows, */
          state = S_NUMDIG; break; } /* go to 'number' state */
        if  (ccl == C_POINT) {       /* if a decimal point follows, */
          state = S_NUMPT; break; }  /* go to fraction state */
        if ((ccl == C_LETTER)        /* if a letter */
        ||  (ccl == C_SIGN)) {       /* or a sign follows, */
          state = S_ID; break; }     /* go to 'identifier' state */
        if ((c == '>')          /* if a '>' follows and previous */
        &&  (scan->value[0] == '-')) {   /* char was a minus sign */
          *p = '\0'; scan->len = 2; return scan->token = T_RGT; }
        UNGETC(scan, c);        /* otherwise put back last character, */
        *--p = '\0';            /* terminate string in buffer */
        scan->len = (int)(p -scan->value); /* set string length */
        return scan->token = T_ID;   /* and return 'identifier' */

      case S_CMPOP:             /* --- comparison operator read */
        if ((c == '-')          /* if a minus sign follows and */
        &&  (scan->value[0] == '<')) {  /* prev. char was a '<' */
          *p++ = '-';       scan->token = T_LFT; }
        else if (c == '=') {    /* if an equal sign follows */
          *p++ = '=';       scan->token = T_CMP; }
        else {                  /* if anything else follows */
          UNGETC(scan, c);  scan->token = scan->value[0]; }
        *p = '\0';              /* terminate string in buffer */
        scan->len = (int)(p -scan->value); /* set string length */
        return scan->token;        /* and return the token read */

      case S_STRING:            /* --- quoted string */
        if ((c == '\n') || (c == EOF))  /* if end of line or file, */
          return scan->token = E_UNTSTR;   /* string is unterminated */
        if (c != quote) {       /* if not at end of string */
          if (p >= end) return scan->token = E_BUFOVF;
          if (c == '\\') {      /* if escaped character follows, */
            state = S_ESC; break; }  /* go to escaped char state */
          *p++ = c; break;      /* otherwise buffer character */
        }                       /* if at end of string, */
        *p = '\0';              /* terminate string in buffer */
        scan->len = (int)(p -scan->value); /* set string length */
        return scan->token = T_ID;   /* and return 'identifier' */

      case S_ESC:               /* --- after '\' in quoted string */
        if ((c >= '0') && (c <= '7')) {        /* if octal digit, */
          ec = c -'0'; state = S_OCT1; break; }/* evaluate digit  */
        if (c == 'x') {         /* if hexadecimal character code, */
          state = S_HEX1; break;} /* go to hexadecimal evaluation */
        switch (c) {            /* evaluate character after '\' */
          case  'a': c = '\a'; break;
          case  'b': c = '\b'; break;
          case  'f': c = '\f'; break;
          case  'n': c = '\n'; break;
          case  'r': c = '\r'; break;
          case  't': c = '\t'; break;
          case  'v': c = '\v'; break;
          case '\n': c = -1;   break;
          default  :           break;
        }                       /* get escaped character */
        if (c >= 0) *p++ = c;   /* and store it, then */
        state = S_STRING; break;/* return to quoted string state */

      case S_OCT1:              /* --- escaped octal number 1 */
        if ((c >= '0')          /* if an octal digit follows, */
        &&  (c <= '7')) {       /* evaluate it */
          ec = ec *8 +c -'0'; state = S_OCT2; break; }
        UNGETC(scan, c);        /* otherwise put back last character */
        *p++  = ec;             /* store escaped character and */
        state = S_STRING; break;/* return to quoted string state */

      case S_OCT2:              /* --- escaped octal number 2 */
        if ((c >= '0') || (c <= '7'))
          ec = ec *8 +c -'0';   /* if octal digit, evaluate it */
        else UNGETC(scan, c);   /* otherwise put back last character */
        *p++  = ec;             /* store escaped character and */
        state = S_STRING; break;/* return to quoted string state */

      case S_HEX1:              /* --- escaped hexadecimal number 1 */
        if (ccl == C_DIGIT) {   /* if hexadecimal digit, evaluate it */
          ec = c -'0';     state = S_HEX2; break; }
        if ((c >= 'a') && (c <= 'f')) {
          ec = c -'a' +10; state = S_HEX2; break; }
        if ((c >= 'A') && (c <= 'F')) {
          ec = c -'A' +10; state = S_HEX2; break; }
        UNGETC(scan, c);        /* otherwise put back last character */
        *p++  = 'x';            /* store escaped character ('x') and */
        state = S_STRING; break;/* return to quoted string state */

      case S_HEX2:              /* --- escaped hexadecimal number 2 */
        if (ccl == C_DIGIT)     /* if hexadecimal digit, evaluate it */
          ec = ec*16 +c -'0';
        else if ((c >= 'a') && (c <= 'f'))
          ec = ec*16 +c -'a' +10;
        else if ((c >= 'A') && (c <= 'F'))
          ec = ec*16 +c -'A' +10;
        else UNGETC(scan, c);   /* otherwise put back last character */
        *p++  = ec;             /* store escaped character and */
        state = S_STRING; break;/* return to quoted string state */

      case S_SLASH:             /* --- slash '/' */
        if (c == '/') {         /* if C++ style comment, then */
          state = S_CPPCOM; break; }   /* skip to end of line */
        if (c == '*') {         /* if C style comment */
          scan->start = scan->line; level = 1;
          state = S_CCOM1; break;    /* note start line, init. level */
        }                       /* and go to first 'comment' state */
        UNGETC(scan, c);        /* otherwise put back last character */
        *p++ = '/'; *p = '\0';  /* store character in buffer */
        scan->len = 1;          /* set string length and */
        return scan->token = '/';  /* return `character' */

      case S_CPPCOM:            /* --- C++ style comment */
        if ((c == '\n')         /* if at end of line */
        ||  (c == EOF))         /* or at end of file */
          state = S_SPACE;      /* return to white space skipping */
        break;                  /* (skip to end of line) */

      case S_CCOM1:             /* --- C style comment 1 */
        if      (c == EOF)      /* if end of file, abort */
          return scan->token = E_UNTCOM;
        if      (c == '*')      /* if possibly 'end of comment', */
          state = S_CCOM2;      /* go to 2nd 'comment' state */
        else if (c == '/')      /* if possibly 'start of comment', */
          state = S_CCOM3;      /* go to 3rd 'comment' state */
        break;

      case S_CCOM2:             /* --- C style comment 2 */
        if      (c == EOF)      /* if end of file, abort */
          return scan->token = E_UNTCOM;
        if      (c == '/') {    /* if end of comment found */
          if (--level <= 0) state = S_SPACE;
          else              state = S_CCOM1; }
        else if (c != '*')      /* if end of comment impossible */
          state = S_CCOM1;      /* return to comment skipping */
        break;                  /* (possible start of comment) */

      case S_CCOM3:             /* --- C style comment 3 */
        if      (c == EOF)      /* if end of file, abort */
          return scan->token = E_UNTCOM;
        if      (c == '*') {    /* if start of comment found */
          level++; state = S_CCOM1; }
        else if (c != '/')      /* if start of comment impossible */
          state = S_CCOM1;      /* return to comment skipping */
        break;                  /* (possible end of comment) */

      default:                  /* if state is illegal, abort */
        return scan->token = E_STATE;

    }  /* switch() */
  }  /* while(1) */
}  /* sc_next() */

/*--------------------------------------------------------------------*/

int sc_nexter (SCAN *scan)
{                               /* --- get next token error reporting */
  if (sc_next(scan) < 0) return sc_error(scan, scan->token);
  return scan->token;           /* get next token, report error, */
}  /* sc_nexter() */            /* and return next token */

/*--------------------------------------------------------------------*/

int sc_back (SCAN *scan)
{                               /* --- go back one token */
  if (scan->back)               /* a second step backwards */
    return scan->token;         /* is impossible, so do nothing */
  scan->back = -1;              /* set the step backward flag */
  return _swap(scan);           /* swap the token information */
}  /* sc_back() */              /* and return the previous token */

/*--------------------------------------------------------------------*/

int sc_eof (SCAN *scan)
{                               /* --- check for end of file */
  if (scan->token == T_EOF) return 1;
  sc_error(scan, E_GARBAGE);    /* check for end of file */
  return 0;                     /* and report an error */
}  /* sc_eof() */               /* if it is not reached */

/*--------------------------------------------------------------------*/

int sc_recover (SCAN *scan, int stop, int beg, int end, int level)
{                               /* --- recover from an error */
  while ((scan->token != stop)     /* while at stop token */
  &&     (scan->token != T_EOF)) { /* and not at end of file */
    if       (scan->token == beg)  /* if begin level token found, */
      level++;                     /* increment level counter */
    else if ((scan->token == end)  /* if end level token found */
    &&       (--level    <= 0))    /* and on level to return to, */
      break;                       /* abort loop */
    if (sc_next(scan) < 0) return scan->token;
  }                             /* consume token */
  if (scan->token != T_EOF)     /* if not at end of file, */
    sc_next(scan);              /* consume token (stop or end) */
  return scan->token;           /* return the next token */
}  /* sc_recover() */

/*--------------------------------------------------------------------*/

void sc_errfile (SCAN *scan, FILE *file, int lncnt)
{                               /* --- set file for error output */
  assert(scan);                 /* check the function arguments */
  scan->errfile = (file) ? file : stderr;
  scan->lncnt   = lncnt;        /* set file and line count */
}  /* sc_errfile() */

/*--------------------------------------------------------------------*/

void sc_errmsgs (SCAN *scan, const char *msgs[], int cnt)
{                                /* --- set additonal error messages */
  assert(scan);                  /* check the function arguments */
  scan->msgs   = msgs;           /* note error message vector */
  scan->msgcnt = cnt;            /* and number of error messages */
}  /* sc_errmsgs() */

/*--------------------------------------------------------------------*/

int sc_error (SCAN *scan, int code, ...)
{                               /* --- print an error message */
  va_list    args;              /* variable argument list */
  const char *msg;              /* error message */
  int        c, pc;             /* the illegal character */
  int        tmp;               /* temporary buffer */

  assert(scan);                  /* check the function arguments */
  if (scan->lncnt <= 0)         /* if line count is zero or negative, */
    putc('\n', scan->errfile);  /* start a new output line */
  fprintf(scan->errfile, FILETXT" %s", scan->fname);
                                /* print the file name */
  if ((code != E_NONE)          /* if an error occurred, */
  &&  (code != E_FOPEN)         /* but not 'file open failed' */
  &&  (code != E_UNTCOM)) {     /* and not 'unterminated comment' */
    fputs((scan->lncnt > 2) ? ",\n" : ", ", scan->errfile);
    fprintf(scan->errfile, LINETXT" %d", scan->line);
  }                             /* print line number */
  fputs((scan->lncnt > 1) ? ":\n" : ": ", scan->errfile);

  if (code >= 0) code = E_NONE; /* check error code and */
  tmp = MSGOFFSET -code;        /* get error message text */
  if      (code > E_UNKNOWN)    msg = _errmsgs[-code];
  else if (tmp  < scan->msgcnt) msg = scan->msgs[tmp];
  else                          msg = NULL;
  if (!msg) msg = _errmsgs[-(code = E_UNKNOWN)];

  switch (code) {               /* special error handling */
    case E_ILLCHR: c = pc = (unsigned char)scan->value[0];
                   if (c < ' ') pc = ' ';
                   fprintf (scan->errfile, msg, pc, c);       break;
    case E_UNTCOM: fprintf (scan->errfile, msg, scan->start); break;
    default      : va_start(args, code); /* get variable arguments */
                   vfprintf(scan->errfile, msg, args);
                   va_end(args); break;  /* print error message and */
  }                             /* end variable argument evaluation */
  if (scan->lncnt > 0)          /* if line count is positive, */
    putc('\n', scan->errfile);  /* terminate output line */
  return code;                  /* return error code */
}  /* sc_error() */

#endif
