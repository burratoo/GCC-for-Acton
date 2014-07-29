/* Support for GCC on Acton
   Patrick Bernardi
 
   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* Set defaults for Acton*/

/* Left here as it may be useful in th future.
#undef	LIB_DEFAULT_SPEC
#define LIB_DEFAULT_SPEC */

/* Default to using software floating point.  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT	(0)
#endif

#undef  LINK_SPEC
#define LINK_SPEC "%{!T*:-Tacton.ld}"

#undef  PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* Return a nonzero value if DECL has a section attribute.  */
#define IN_NAMED_SECTION_P(DECL)					\
  ((TREE_CODE (DECL) == FUNCTION_DECL || TREE_CODE (DECL) == VAR_DECL)	\
   && DECL_SECTION_NAME (DECL) != NULL_TREE)

#undef  ASM_OUTPUT_ALIGNED_BSS
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN)   	\
  do									\
    {									\
      if (IN_NAMED_SECTION_P (DECL))					\
	switch_to_section (get_named_section (DECL, NULL, 0));		\
      else								\
	switch_to_section (bss_section);				\
      									\
      ASM_OUTPUT_ALIGN (FILE, floor_log2 (ALIGN / BITS_PER_UNIT));	\
									\
      last_assemble_variable_decl = DECL;				\
      ASM_DECLARE_OBJECT_NAME (FILE, NAME, DECL);			\
      ASM_OUTPUT_SKIP (FILE, SIZE ? (int)(SIZE) : 1);			\
    } 									\
  while (0)

#undef  ASM_OUTPUT_ALIGNED_DECL_LOCAL
#define ASM_OUTPUT_ALIGNED_DECL_LOCAL(FILE, DECL, NAME, SIZE, ALIGN)	\
  do									\
    {									\
      if ((DECL) != NULL && IN_NAMED_SECTION_P (DECL))			\
	switch_to_section (get_named_section (DECL, NULL, 0));		\
      else								\
	switch_to_section (bss_section);				\
									\
      ASM_OUTPUT_ALIGN (FILE, floor_log2 (ALIGN / BITS_PER_UNIT));	\
      ASM_OUTPUT_LABEL (FILE, NAME);					\
      fprintf (FILE, "\t.space\t%d\n", SIZE ? (int)(SIZE) : 1);		\
    }									\
  while (0)

#ifndef SUBTARGET_CPU_DEFAULT
#define SUBTARGET_CPU_DEFAULT 		TARGET_CPU_arm7tdmi
#endif
