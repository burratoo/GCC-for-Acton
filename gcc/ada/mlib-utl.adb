------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             M L I B . U T L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2002-2014, AdaCore                     --
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

with MLib.Fil; use MLib.Fil;
with MLib.Tgt; use MLib.Tgt;
with Opt;
with Osint;
with Output;   use Output;

with Interfaces.C.Strings; use Interfaces.C.Strings;

package body MLib.Utl is

   Adalib_Path : String_Access := null;
   --  Path of the GNAT adalib directory, specified in procedure
   --  Specify_Adalib_Dir. Used in function Lib_Directory.

   Gcc_Name : String_Access;
   --  Default value of the "gcc" executable used in procedure Gcc

   Gcc_Exec : String_Access;
   --  The full path name of the "gcc" executable

   Ar_Name : String_Access;
   --  The name of the archive builder for the platform, set when procedure Ar
   --  is called for the first time.

   Ar_Exec : String_Access;
   --  The full path name of the archive builder

   Ar_Options : String_List_Access;
   --  The minimum options used when invoking the archive builder

   Ar_Append_Options : String_List_Access;
   --  The options to be used when invoking the archive builder to add chunks
   --  of object files, when building the archive in chunks.

   Opt_Length : Natural := 0;
   --  The max number of options for the Archive_Builder

   Initial_Size : Natural := 0;
   --  The minimum number of bytes for the invocation of the Archive Builder
   --  (without name of the archive or object files).

   Ranlib_Name : String_Access;
   --  The name of the archive indexer for the platform, if there is one

   Ranlib_Exec : String_Access := null;
   --  The full path name of the archive indexer

   Ranlib_Options : String_List_Access := null;
   --  The options to be used when invoking the archive indexer, if any

   --------
   -- Ar --
   --------

   procedure Ar (Output_File : String; Objects : Argument_List) is
      Full_Output_File : constant String :=
                             Ext_To (Output_File, Archive_Ext);

      Arguments   : Argument_List_Access;
      Last_Arg    : Natural := 0;
      Success     : Boolean;
      Line_Length : Natural := 0;

      Maximum_Size : Integer;
      pragma Import (C, Maximum_Size, "__gnat_link_max");
      --  Maximum number of bytes to put in an invocation of the
      --  Archive_Builder.

      Size : Integer;
      --  The number of bytes for the invocation of the archive builder

      Current_Object : Natural;

      procedure Display;
      --  Display an invocation of the Archive Builder

      -------------
      -- Display --
      -------------

      procedure Display is
      begin
         if not Opt.Quiet_Output then
            Write_Str (Ar_Name.all);
            Line_Length := Ar_Name'Length;

            for J in 1 .. Last_Arg loop

               --  Make sure the Output buffer does not overflow

               if Line_Length + 1 + Arguments (J)'Length > Buffer_Max then
                  Write_Eol;
                  Line_Length := 0;
               end if;

               Write_Char (' ');

               --  Only output the first object files when not in verbose mode

               if (not Opt.Verbose_Mode) and then J = Opt_Length + 3 then
                  Write_Str ("...");
                  exit;
               end if;

               Write_Str (Arguments (J).all);
               Line_Length := Line_Length + 1 + Arguments (J)'Length;
            end loop;

            Write_Eol;
         end if;

      end Display;

   begin
      if Ar_Exec = null then
         Ar_Name := Osint.Program_Name (Archive_Builder, "gnatmake");
         Ar_Exec := Locate_Exec_On_Path (Ar_Name.all);

         if Ar_Exec = null then
            Free (Ar_Name);
            Ar_Name := new String'(Archive_Builder);
            Ar_Exec := Locate_Exec_On_Path (Ar_Name.all);
         end if;

         if Ar_Exec = null then
            Fail (Ar_Name.all & " not found in path");

         elsif Opt.Verbose_Mode then
            Write_Str  ("found ");
            Write_Line (Ar_Exec.all);
         end if;

         Ar_Options := Archive_Builder_Options;

         Initial_Size := 0;
         for J in Ar_Options'Range loop
            Initial_Size := Initial_Size + Ar_Options (J)'Length + 1;
         end loop;

         Ar_Append_Options := Archive_Builder_Append_Options;

         Opt_Length := Ar_Options'Length;

         if Ar_Append_Options /= null then
            Opt_Length := Natural'Max (Ar_Append_Options'Length, Opt_Length);

            Size := 0;
            for J in Ar_Append_Options'Range loop
               Size := Size + Ar_Append_Options (J)'Length + 1;
            end loop;

            Initial_Size := Integer'Max (Initial_Size, Size);
         end if;

         --  ranlib

         Ranlib_Name := Osint.Program_Name (Archive_Indexer, "gnatmake");

         if Ranlib_Name'Length > 0 then
            Ranlib_Exec := Locate_Exec_On_Path (Ranlib_Name.all);

            if Ranlib_Exec = null then
               Free (Ranlib_Name);
               Ranlib_Name := new String'(Archive_Indexer);
               Ranlib_Exec := Locate_Exec_On_Path (Ranlib_Name.all);
            end if;

            if Ranlib_Exec /= null and then Opt.Verbose_Mode then
               Write_Str ("found ");
               Write_Line (Ranlib_Exec.all);
            end if;
         end if;

         Ranlib_Options := Archive_Indexer_Options;
      end if;

      Arguments :=
        new String_List (1 .. 1 + Opt_Length + Objects'Length);
      Arguments (1 .. Ar_Options'Length) := Ar_Options.all; --  "ar cr ..."
      Arguments (Ar_Options'Length + 1) := new String'(Full_Output_File);

      Delete_File (Full_Output_File);

      Size := Initial_Size + Full_Output_File'Length + 1;

      --  Check the full size of a call of the archive builder with all the
      --  object files.

      for J in Objects'Range loop
         Size := Size + Objects (J)'Length + 1;
      end loop;

      --  If the size is not too large or if it is not possible to build the
      --  archive in chunks, build the archive in a single invocation.

      if Size <= Maximum_Size or else Ar_Append_Options = null then
         Last_Arg := Ar_Options'Length + 1 + Objects'Length;
         Arguments (Ar_Options'Length + 2 .. Last_Arg) := Objects;

         Display;

         Spawn (Ar_Exec.all, Arguments (1 .. Last_Arg), Success);

      else
         --  Build the archive in several invocation, making sure to not
         --  go over the maximum size for each invocation.

         Last_Arg := Ar_Options'Length + 1;
         Current_Object := Objects'First;
         Size := Initial_Size + Full_Output_File'Length + 1;

         --  First invocation

         while Current_Object <= Objects'Last loop
            Size := Size + Objects (Current_Object)'Length + 1;
            exit when Size > Maximum_Size;
            Last_Arg := Last_Arg + 1;
            Arguments (Last_Arg) := Objects (Current_Object);
            Current_Object := Current_Object + 1;
         end loop;

         Display;

         Spawn (Ar_Exec.all, Arguments (1 .. Last_Arg), Success);

         Arguments (1 .. Ar_Append_Options'Length) := Ar_Append_Options.all;
         Arguments
           (Ar_Append_Options'Length + 1) := new String'(Full_Output_File);

         --  Appending invocation(s)

         Big_Loop : while Success and then Current_Object <= Objects'Last loop
            Last_Arg := Ar_Append_Options'Length + 1;
            Size := Initial_Size + Full_Output_File'Length + 1;

            Inner_Loop : while Current_Object <= Objects'Last loop
               Size := Size + Objects (Current_Object)'Length + 1;
               exit Inner_Loop when Size > Maximum_Size;
               Last_Arg := Last_Arg + 1;
               Arguments (Last_Arg) := Objects (Current_Object);
               Current_Object := Current_Object + 1;
            end loop Inner_Loop;

            Display;

            Spawn (Ar_Exec.all, Arguments (1 .. Last_Arg), Success);
         end loop Big_Loop;
      end if;

      if not Success then
         Fail (Ar_Name.all & " execution error.");
      end if;

      --  If we have found ranlib, run it over the library

      if Ranlib_Exec /= null then
         if not Opt.Quiet_Output then
            Write_Str  (Ranlib_Name.all);
            Write_Char (' ');

            for J in Ranlib_Options'Range loop
               Write_Str  (Ranlib_Options (J).all);
               Write_Char (' ');
            end loop;

            Write_Line (Arguments (Ar_Options'Length + 1).all);
         end if;

         Spawn
           (Ranlib_Exec.all,
            Ranlib_Options.all & (Arguments (Ar_Options'Length + 1)),
            Success);

         if not Success then
            Fail (Ranlib_Name.all & " execution error.");
         end if;
      end if;
   end Ar;

   -----------------
   -- Delete_File --
   -----------------

   procedure Delete_File (Filename : String) is
      File    : constant String := Filename & ASCII.NUL;
      Success : Boolean;

   begin
      Delete_File (File'Address, Success);

      if Opt.Verbose_Mode then
         if Success then
            Write_Str ("deleted ");

         else
            Write_Str ("could not delete ");
         end if;

         Write_Line (Filename);
      end if;
   end Delete_File;

   ---------
   -- Gcc --
   ---------

   procedure Gcc
     (Output_File : String;
      Objects     : Argument_List;
      Options     : Argument_List;
      Options_2   : Argument_List;
      Driver_Name : Name_Id := No_Name)
   is
      Link_Bytes : Integer := 0;
      --  Projected number of bytes for the linker command line

      Link_Max : Integer;
      pragma Import (C, Link_Max, "__gnat_link_max");
      --  Maximum number of bytes on the command line supported by the OS
      --  linker. Passed this limit the response file mechanism must be used
      --  if supported.

      Object_List_File_Supported : Boolean;
      for Object_List_File_Supported'Size use Character'Size;
      pragma Import
        (C, Object_List_File_Supported, "__gnat_objlist_file_supported");
      --  Predicate indicating whether the linker has an option whereby the
      --  names of object files can be passed to the linker in a file.

      Object_File_Option_Ptr : Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Object_File_Option_Ptr, "__gnat_object_file_option");
      --  Pointer to a string representing the linker option which specifies
      --  the response file.

      Object_File_Option : constant String := Value (Object_File_Option_Ptr);
      --  The linker option which specifies the response file as a string

      Using_GNU_response_file : constant Boolean :=
                                  Object_File_Option'Length > 0
                                    and then
                                      Object_File_Option
                                        (Object_File_Option'Last) = '@';
      --  Whether a GNU response file is used

      Tname    : String_Access;
      Tname_FD : File_Descriptor := Invalid_FD;
      --  Temporary file used by linker to pass list of object files on
      --  certain systems with limitations on size of arguments.

      Closing_Status : Boolean;
      --  For call to Close

      Arguments :
        Argument_List
          (1 .. 7 + Objects'Length + Options'Length + Options_2'Length);

      A       : Natural := 0;
      Success : Boolean;

      Out_Opt : constant String_Access := new String'("-o");
      Out_V   : constant String_Access := new String'(Output_File);
      Lib_Dir : constant String_Access := new String'("-L" & Lib_Directory);
      Lib_Opt : constant String_Access := new String'(Dynamic_Option);

      Driver : String_Access;

      type Object_Position is (First, Second, Last);

      Position : Object_Position;

      procedure Write_RF (S : String);
      --  Write a string to the response file and check if it was successful.
      --  Fail the program if it was not successful (disk full).

      --------------
      -- Write_RF --
      --------------

      procedure Write_RF (S : String) is
         Success    : Boolean            := True;
         Back_Slash : constant Character := '\';

      begin
         --  If a GNU response file is used, space and backslash need to be
         --  escaped because they are interpreted as a string separator and
         --  an escape character respectively by the underlying mechanism.
         --  On the other hand, quote and double-quote are not escaped since
         --  they are interpreted as string delimiters on both sides.

         if Using_GNU_response_file then
            for J in S'Range loop
               if S (J) = ' ' or else S (J) = '\' then
                  if Write (Tname_FD, Back_Slash'Address, 1) /= 1 then
                     Success := False;
                  end if;
               end if;

               if Write (Tname_FD, S (J)'Address, 1) /= 1 then
                  Success := False;
               end if;
            end loop;

         else
            if Write (Tname_FD, S'Address, S'Length) /= S'Length then
               Success := False;
            end if;
         end if;

         if Write (Tname_FD, ASCII.LF'Address, 1) /= 1 then
            Success := False;
         end if;

         if not Success then
            Fail ("cannot generate response file to link library: disk full");
         end if;
      end Write_RF;

   --  Start of processing for Gcc

   begin
      if Driver_Name = No_Name then
         if Gcc_Exec = null then
            if Gcc_Name = null then
               Gcc_Name := Osint.Program_Name ("gcc", "gnatmake");
            end if;

            Gcc_Exec := Locate_Exec_On_Path (Gcc_Name.all);

            if Gcc_Exec = null then
               Fail (Gcc_Name.all & " not found in path");
            end if;
         end if;

         Driver := Gcc_Exec;

      else
         Driver := Locate_Exec_On_Path (Get_Name_String (Driver_Name));

         if Driver = null then
            Fail (Get_Name_String (Driver_Name) & " not found in path");
         end if;
      end if;

      Link_Bytes := 0;

      if Lib_Opt'Length /= 0 then
         A := A + 1;
         Arguments (A) := Lib_Opt;
         Link_Bytes := Link_Bytes + Lib_Opt'Length + 1;
      end if;

      A := A + 1;
      Arguments (A) := Out_Opt;
      Link_Bytes := Link_Bytes + Out_Opt'Length + 1;

      A := A + 1;
      Arguments (A) := Out_V;
      Link_Bytes := Link_Bytes + Out_V'Length + 1;

      A := A + 1;
      Arguments (A) := Lib_Dir;
      Link_Bytes := Link_Bytes + Lib_Dir'Length + 1;

      A := A + Options'Length;
      Arguments (A - Options'Length + 1 .. A) := Options;

      for J in Options'Range loop
         Link_Bytes := Link_Bytes + Options (J)'Length + 1;
      end loop;

      if not Opt.Quiet_Output then
         if Opt.Verbose_Mode then
            Write_Str (Driver.all);

         elsif Driver_Name /= No_Name then
            Write_Str (Get_Name_String (Driver_Name));

         else
            Write_Str (Gcc_Name.all);
         end if;

         for J in 1 .. A loop
            if Opt.Verbose_Mode or else J < 4 then
               Write_Char (' ');
               Write_Str  (Arguments (J).all);

            else
               Write_Str (" ...");
               exit;
            end if;
         end loop;

         --  Do not display all the object files if not in verbose mode, only
         --  the first one.

         Position := First;
         for J in Objects'Range loop
            if Opt.Verbose_Mode or else Position = First then
               Write_Char (' ');
               Write_Str (Objects (J).all);
               Position := Second;

            elsif Position = Second then
               Write_Str (" ...");
               Position := Last;
               exit;
            end if;
         end loop;

         for J in Options_2'Range loop
            if not Opt.Verbose_Mode then
               if Position = Second then
                  Write_Str (" ...");
               end if;

               exit;
            end if;

            Write_Char (' ');
            Write_Str (Options_2 (J).all);
         end loop;

         Write_Eol;
      end if;

      for J in Objects'Range loop
         Link_Bytes := Link_Bytes + Objects (J)'Length + 1;
      end loop;

      for J in Options_2'Range loop
         Link_Bytes := Link_Bytes + Options_2 (J)'Length + 1;
      end loop;

      if Object_List_File_Supported and then Link_Bytes > Link_Max then

         --  Create a temporary file containing the object files, one object
         --  file per line for maximal compatibility with linkers supporting
         --  this option.

         Create_Temp_File (Tname_FD, Tname);

         for J in Objects'Range loop
            Write_RF (Objects (J).all);
         end loop;

         Close (Tname_FD, Closing_Status);

         if not Closing_Status then
            Fail ("cannot generate response file to link library: disk full");
         end if;

         A := A + 1;
         Arguments (A) := new String'(Object_File_Option & Tname.all);

      else
         A := A + Objects'Length;
         Arguments (A - Objects'Length + 1 .. A) := Objects;
      end if;

      A := A + Options_2'Length;
      Arguments (A - Options_2'Length + 1 .. A) := Options_2;

      Spawn (Driver.all, Arguments (1 .. A), Success);

      if Success then
         --  Delete the temporary file used in conjunction with linking
         --  if one was created.

         if Tname_FD /= Invalid_FD then
            Delete_File (Tname.all);
         end if;

      else
         if Driver_Name = No_Name then
            Fail (Gcc_Name.all & " execution error");
         else
            Fail (Get_Name_String (Driver_Name) & " execution error");
         end if;
      end if;
   end Gcc;

   -------------------
   -- Lib_Directory --
   -------------------

   function Lib_Directory return String is
      Libacton : constant String := Tgt.Libacton;

   begin
      --  If procedure Specify_Adalib_Dir has been called, used the specified
      --  value.

      if Adalib_Path /= null then
         return Adalib_Path.all;
      end if;

      Name_Len := Libacton'Length;
      Name_Buffer (1 .. Name_Len) := Libacton;
      Get_Name_String (Osint.Find_File (Name_Enter, Osint.Library));

      --  Remove libacton.a

      return Name_Buffer (1 .. Name_Len - Libacton'Length);
   end Lib_Directory;

   ------------------------
   -- Specify_Adalib_Dir --
   ------------------------

   procedure Specify_Adalib_Dir (Path : String) is
   begin
      if Path'Length = 0 then
         Adalib_Path := null;
      else
         Adalib_Path := new String'(Path);
      end if;
   end Specify_Adalib_Dir;

end MLib.Utl;
