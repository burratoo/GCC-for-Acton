------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              B I N D G E N                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the routines to output the binder file. This is
--  an Ada program which contains the following:

--     Initialization for main program case
--     Sequence of calls to elaboration routines in appropriate order
--     Call to main program for main program case

--  See the body for exact details of the file that is generated

with Namet;    use Namet;
with Table;    use Table;

package Bindgen is

   procedure Gen_Output_File (Filename : String);
   --  Filename is the full path name of the binder output file

   ----------------------------------
   -- Unique_Dispatching_Policies  --
   ----------------------------------

   --  Table to store each unique policy. Put here as it is shared with
   --  gnatbinder.

   package Unique_Dispatching_Policies is new Table.Table
     (Table_Component_Type => Name_Id,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 10,
      Table_Name           => "Gnatbind.Scheduler_Agents");

end Bindgen;
