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

#undef	STARTFILE_DEFAULT_SPEC

#undef	LINK_START_DEFAULT_SPEC

/* Acton does not require any startup file */

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC ""

#undef  LINK_SPEC
#define LINK_SPEC "%{!T*:-Tacton.ld}"

/* Suppress the generation of -lgcc and -lc flags cause we're awesome. */
#undef  LINK_GCC_C_SEQUENCE_SPEC 
#define LINK_GCC_C_SEQUENCE_SPEC "%G"