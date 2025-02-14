// A wrapper for opus_compare.c which renames the main symbol to avoid conflits
// with Haskell, and which disables the use of fprintf.
#define main opus_compare_main
#define fprintf(out,fmt,...)
#include "opus_compare.c"
#undef main
#undef fprintf
