/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target arm_eabi } } */
/* { dg-require-effective-target arm_neon_fp16_ok } */
/* { dg-options "-O -mfpu=vfp -mfloat-abi=hard -mfp16-format=ieee" } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "vfp21.c"

#define PCSATTR __attribute__((pcs("aapcs")))

#include "abitest.h"
#else
#if defined (__ARM_BIG_ENDIAN)
ARG (__fp16, 1.0f, R0 + 2)
#else
ARG (__fp16, 1.0f, R0)
#endif
ARG (double, 2.0, R2)
ARG (__fp16, 3.0f, STACK)
ARG (float, 2.0f, STACK+4)
LAST_ARG (double, 4.0, STACK+8)
#endif

