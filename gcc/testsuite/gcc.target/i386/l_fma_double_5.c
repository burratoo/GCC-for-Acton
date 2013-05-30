/* { dg-do compile } */
/* { dg-options "-O3 -Wno-attributes -mfpmath=sse -mfma -mtune=generic" } */

/* Test that the compiler properly optimizes floating point multiply
   and add instructions into FMA3 instructions.  */

typedef double adouble __attribute__((aligned(sizeof (double))));
#define TYPE adouble

#include "l_fma_5.h"

/* { dg-final { scan-assembler-times "vfmadd132pd" 8  } } */
/* { dg-final { scan-assembler-times "vfmsub132pd" 8  } } */
/* { dg-final { scan-assembler-times "vfnmadd132pd" 8  } } */
/* { dg-final { scan-assembler-times "vfnmsub132pd" 8  } } */
/* { dg-final { scan-assembler-times "vfmadd132sd" 56 } } */
/* { dg-final { scan-assembler-times "vfmsub132sd" 56  } } */
/* { dg-final { scan-assembler-times "vfnmadd132sd" 56  } } */
/* { dg-final { scan-assembler-times "vfnmsub132sd" 56  } } */
