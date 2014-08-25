/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpcmpuq\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\n^k\]*%k\[0-9\]\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpuq\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\n^k\]*%k\[0-9\]\{" 1 } } */

#include <immintrin.h>

volatile __m512i x;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  m = _mm512_cmple_epu64_mask (x, x);
  m = _mm512_mask_cmple_epu64_mask (m, x, x);
}
