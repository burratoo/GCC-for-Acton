------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  O P T                                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2010, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains global flags set by the initialization routine from
--  the command line and referenced throughout the compiler, the binder, or
--  other GNAT tools. The comments indicate which options are used by which
--  programs (GNAT, GNATBIND, GNATLINK, GNATMAKE, GPRMAKE, etc).

--  Some flags are labelled "PROJECT MANAGER". These are used by tools that
--  use the Project Manager. These tools include gnatmake, gnatname, the gnat
--  driver, gnatclean, gprbuild and gprclean.

--  This specific package handles option for the Table to remove an elaboration
--  circuilarity.

package Opt.Table is

   Table_Factor : Int := 1;
   --  GNAT
   --  Factor by which all initial table sizes set in Alloc are multiplied.
   --  Used in Table to calculate initial table sizes (the initial table size
   --  is the value in Alloc, used as the Table_Initial parameter value,
   --  multiplied by the factor given here. The default value is used if no
   --  -gnatT switch appears.
end Opt.Table;
