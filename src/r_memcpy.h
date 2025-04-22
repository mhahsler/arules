// Code from r-lib/rlang (see: https://github.com/r-lib/rlang/pull/1797)

#ifndef R_MEMCPY_H
#define R_MEMCPY_H

#include <string.h>

// Slightly safer version of `memcpy()` for use with R object memory
//
// Prefer this over `memcpy()`, especially when providing pointers to R object
// memory. As of R 4.5.0, `DATAPTR()` and friends return `(void*) 1` on 0-length
// R objects, so we must be extremely careful to never use dereference those
// pointers. In particular, it is not safe to call `memcpy(dest, src, 0)` on
// some machines (likely with sanitizers active) when either `dest` or `src`
// resolve to `(void*) 1`.
//
// https://github.com/r-lib/vctrs/pull/1968
// https://github.com/r-devel/r-svn/blob/9976c3d7f08c754593d01ba8380afb6be803dde2/src/main/memory.c#L4137-L4150
static inline
void r_memcpy(void* dest, const void* src, size_t count) {
  if (count) {
    memcpy(dest, src, count);
  }
}

// Slightly safer version of `memset()` for use with R object memory
//
// See `r_memcpy()` for rationale
static inline
void r_memset(void* dest, int value, size_t count) {
  if (count) {
    memset(dest, value, count);
  }
}

#endif
