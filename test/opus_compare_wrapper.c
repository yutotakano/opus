// A wrapper for opus_compare.c which renames the main symbol to avoid conflits
// with Haskell, and which disables the use of fprintf.

// First, include stdio to prevent the fprintf definition later from breaking
// the definition of fprintf
#include <stdio.h>

// Rewrite main to opus_compare_main to prevent conflits when FFI-ed
#define main opus_compare_main

// Rewrite all fprintf to a no-op
#define fprintf(out,fmt,...)
#include "opus_compare.c"

// Undefine main and fprintf for safety
#undef main
#undef fprintf
