/* HAVE_AS_DWARF2_DEBUG_LINE macro needs to be defined to pass the unittest.
   However, there dg cannot defin it as, so we restrict the target to linux.  */
/* { dg-do compile { target *-*-linux-gnu } } */
/* { dg-additional-options "-O0" } */
/* { dg-final { scan-assembler "loc \[0-9] 11 \[0-9]( is_stmt \[0-9])?\n" } } */
/* { dg-final { scan-assembler "loc \[0-9] 11 \[0-9]( is_stmt \[0-9])? discriminator 2\n" } } */
/* { dg-final { scan-assembler "loc \[0-9] 11 \[0-9]( is_stmt \[0-9])? discriminator 1\n" } } */

int foo(int n) {
  int i, ret = 0;
  for (i = 0; i < n; i++) {
    if (i % 10 == 1)
      ret++;
    else
      ret--;
  }
  return ret;
}
