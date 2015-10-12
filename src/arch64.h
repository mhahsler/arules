/*----------------------------------------------------------------------
  Detect 64 bit architecure
  ----------------------------------------------------------------------*/

/*
 * Type	    ILP64   LP64    LLP64
 * int	    64	    32	    32
 * long	    64	    64	    32
 * pointer  64	    64	    64
 * long long64	    64	    64
 * 
 * Windows is LLP64
 * Linux et al is LP64
 */

#if defined(__LP64__) || defined(_LP64)
#define ARCH64
#endif

