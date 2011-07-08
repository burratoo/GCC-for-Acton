------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              B I N D G E N                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2010, Free Software Foundation, Inc.         --
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

with ALI;      use ALI;
with Binde;    use Binde;
with Casing;   use Casing;
with Fname;    use Fname;
with Gnatvsn;  use Gnatvsn;
with Hostparm;
with Opt;      use Opt;
with Osint;    use Osint;
with Osint.B;  use Osint.B;
with Output;   use Output;
with Rident;   use Rident;
with Targparm; use Targparm;
with Types;    use Types;

with System.OS_Lib;  use System.OS_Lib;
with System.WCh_Con; use System.WCh_Con;

with GNAT.Heap_Sort_A; use GNAT.Heap_Sort_A;

package body Bindgen is

   Statement_Buffer : String (1 .. 1000);
   --  Buffer used for constructing output statements

   Last : Natural := 0;
   --  Last location in Statement_Buffer currently set

   With_DECGNAT : Boolean := False;
   --  Flag which indicates whether the program uses the DECGNAT library
   --  (presence of the unit DEC).

   With_GNARL : Boolean := False;
   --  Flag which indicates whether the program uses the GNARL library
   --  (presence of the unit System.OS_Interface)

   Num_Elab_Calls : Nat := 0;
   --  Number of generated calls to elaboration routines

   System_Restrictions_Used : Boolean;
   --  Flag indicating whether the unit System.Restrictions is in the closure
   --  of the partition. This is set by Check_System_Restrictions_Used, and
   --  is used to determine whether or not to initialize the restrictions
   --  information in the body of the binder generated file (we do not want
   --  to do this unconditionally, since it drags in the System.Restrictions
   --  unit unconditionally, which is unpleasand, especially for ZFP etc.)

   ----------------------------------
   -- Interface_State Pragma Table --
   ----------------------------------

   --  This table assembles the interface state pragma information from
   --  all the units in the partition. Note that Bcheck has already checked
   --  that the information is consistent across units. The entries
   --  in this table are n/u/r/s for not set/user/runtime/system.

   package IS_Pragma_Settings is new Table.Table (
     Table_Component_Type => Character,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => 100,
     Table_Increment      => 200,
     Table_Name           => "IS_Pragma_Settings");

   ----------------------------
   -- Scheduler_Agents Table --
   ----------------------------

   --  Table to record each scheduler agent and their associated priorities.
   --  Used to form create calls to scheduler agents.

   package Scheduler_Agents is new Table.Table
     (Table_Component_Type => Specific_Dispatching_Record,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => System.Any_Priority'First,
      Table_Initial        => 10,
      Table_Increment      => 10,
      Table_Name           => "Scheduler_Agents");

   ----------------------
   -- Run-Time Globals --
   ----------------------

   --  This section documents the global variables that set from the
   --  generated binder file.

   --     Heap_Size                     : Natural;
   --     WC_Encoding                   : Character;
   --     Locking_Policy                : Character;
   --     Queuing_Policy                : Character;
   --     Restrictions                  : System.Address;
   --     Interrupt_States              : System.Address;
   --     Num_Interrupt_States          : Integer;
   --     Unreserve_All_Interrupts      : Integer;
   --     Exception_Tracebacks          : Integer;
   --     Zero_Cost_Exceptions          : Integer;
   --     Detect_Blocking               : Integer;
   --     Default_Stack_Size            : Integer;
   --     Leap_Seconds_Support          : Integer;

   --  Heap_Size is the heap to use for memory allocations set by use of a
   --  -Hnn parameter for the binder or by the GNAT$NO_MALLOC_64 logical.
   --  Valid values are 32 and 64. This switch is only effective on VMS.

   --  WC_Encoding shows the wide character encoding method used for the main
   --  program. This is one of the encoding letters defined in
   --  System.WCh_Con.WC_Encoding_Letters.

   --  Locking_Policy is a space if no locking policy was specified for the
   --  partition. If a locking policy was specified, the value is the upper
   --  case first character of the locking policy name, for example, 'C' for
   --  Ceiling_Locking.

   --  Queuing_Policy is a space if no queuing policy was specified for the
   --  partition. If a queuing policy was specified, the value is the upper
   --  case first character of the queuing policy name for example, 'F' for
   --  FIFO_Queuing.

   --  Restrictions is the address of a null-terminated string specifying the
   --  restrictions information for the partition. The format is identical to
   --  that of the parameter string found on R lines in ali files (see Lib.Writ
   --  spec in lib-writ.ads for full details). The difference is that in this
   --  context the values are the cumulative ones for the entire partition.

   --  Interrupt_States is the address of a string used to specify the
   --  cumulative results of Interrupt_State pragmas used in the partition.
   --  The length of this string is determined by the last interrupt for which
   --  such a pragma is given (the string will be a null string if no pragmas
   --  were used). If pragma were present the entries apply to the interrupts
   --  in sequence from the first interrupt, and are set to one of four
   --  possible settings: 'n' for not specified, 'u' for user, 'r' for run
   --  time, 's' for system, see description of Interrupt_State pragma for
   --  further details.

   --  Num_Interrupt_States is the length of the Interrupt_States string. It
   --  will be set to zero if no Interrupt_State pragmas are present.

   --  Unreserve_All_Interrupts is set to one if at least one unit in the
   --  partition had a pragma Unreserve_All_Interrupts, and zero otherwise.

   --  Exception_Tracebacks is set to one if the -E parameter was present
   --  in the bind and to zero otherwise. Note that on some targets exception
   --  tracebacks are provided by default, so a value of zero for this
   --  parameter does not necessarily mean no trace backs are available.

   --  Zero_Cost_Exceptions is set to one if zero cost exceptions are used for
   --  this partition, and to zero if longjmp/setjmp exceptions are used.

   --  Detect_Blocking indicates whether pragma Detect_Blocking is active or
   --  not. A value of zero indicates that the pragma is not present, while a
   --  value of 1 signals its presence in the partition.

   --  Default_Stack_Size is the default stack size used when creating an Ada
   --  task with no explicit Storage_Size clause.

   --  Leap_Seconds_Support denotes whether leap seconds have been enabled or
   --  disabled. A value of zero indicates that leap seconds are turned "off",
   --  while a value of one signifies "on" status.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure WBI (Info : String) renames Osint.B.Write_Binder_Info;
   --  Convenient shorthand used throughout

   procedure Check_System_Restrictions_Used;
   --  Sets flag System_Restrictions_Used (Set to True if and only if the unit
   --  System.Restrictions is present in the partition, otherwise False).

   procedure Gen_Adainit_Ada;
   --  Generates the Adainit procedure (Ada code case)

   procedure Gen_Adafinal_Ada;
   --  Generate the Adafinal procedure (Ada code case)

   procedure Gen_Elab_Calls_Ada;
   --  Generate sequence of elaboration calls (Ada code case)

   procedure Gen_Elab_Order_Ada;
   --  Generate comments showing elaboration order chosen (Ada case)

   procedure Gen_Main_Ada;
   --  Generate procedure main (Ada code case)

   procedure Gen_Object_Files_Options;
   --  Output comments containing a list of the full names of the object
   --  files to be linked and the list of linker options supplied by
   --  Linker_Options pragmas in the source. (C and Ada code case)

   procedure Gen_Output_File_Ada (Filename : String);
   --  Generate output file (Ada code case)

   procedure Gen_Restrictions_Ada;
   --  Generate initialization of restrictions variable (Ada code case)

   procedure Gen_Versions_Ada;
   --  Output series of definitions for unit versions (Ada code case)

   function Get_Ada_Main_Name return String;
   --  This function is used in the Ada main output case to compute a usable
   --  name for the generated main program. The normal main program name is
   --  Ada_Main, but this won't work if the user has a unit with this name.
   --  This function tries Ada_Main first, and if there is such a clash, then
   --  it tries Ada_Name_01, Ada_Name_02 ... Ada_Name_99 in sequence.

   function Get_Main_Unit_Name (S : String) return String;
   --  Return the main unit name corresponding to S by replacing '.' with '_'

   function Get_Main_Name return String;
   --  This function is used in the Ada main output case to compute the
   --  correct external main program. It is "main" by default, unless the
   --  flag Use_Ada_Main_Program_Name_On_Target is set, in which case it
   --  is the name of the Ada main name without the "_ada". This default
   --  can be overridden explicitly using the -Mname binder switch.

   function Get_WC_Encoding return Character;
   --  Return wide character encoding method to set as WC_Encoding in output.
   --  If -W has been used, returns the specified encoding, otherwise returns
   --  the encoding method used for the main program source. If there is no
   --  main program source (-z switch used), returns brackets ('b').

   function Lt_Linker_Option (Op1, Op2 : Natural) return Boolean;
   --  Compare linker options, when sorting, first according to
   --  Is_Internal_File (internal files come later) and then by
   --  elaboration order position (latest to earliest).

   procedure Move_Linker_Option (From : Natural; To : Natural);
   --  Move routine for sorting linker options

   procedure Resolve_Binder_Options;
   --  Set the value of With_GNARL and With_DECGNAT. The latter only on VMS
   --  since it tests for a package named "dec" which might cause a conflict
   --  on non-VMS systems.

   procedure Set_Char (C : Character);
   --  Set given character in Statement_Buffer at the Last + 1 position
   --  and increment Last by one to reflect the stored character.

   procedure Set_Int (N : Int);
   --  Set given value in decimal in Statement_Buffer with no spaces
   --  starting at the Last + 1 position, and updating Last past the value.
   --  A minus sign is output for a negative value.

   procedure Set_Boolean (B : Boolean);
   --  Set given boolean value in Statement_Buffer at the Last + 1 position
   --  and update Last past the value.

   procedure Set_IS_Pragma_Table;
   --  Initializes contents of IS_Pragma_Settings table from ALI table

   procedure Set_Main_Program_Name;
   --  Given the main program name in Name_Buffer (length in Name_Len)
   --  generate the name of the routine to be used in the call. The name
   --  is generated starting at Last + 1, and Last is updated past it.

   procedure Set_Name_Buffer;
   --  Set the value stored in positions 1 .. Name_Len of the Name_Buffer

   procedure Set_Scheduler_Agent_Table;
   --  Generates the Scheduler_Agents table from the data in the
   --  Specific_Dispatching table.

   procedure Set_String (S : String);
   --  Sets characters of given string in Statement_Buffer, starting at the
   --  Last + 1 position, and updating last past the string value.

   procedure Set_String_Replace (S : String);
   --  Replaces the last S'Length characters in the Statement_Buffer with
   --  the characters of S. The caller must ensure that these characters do
   --  in fact exist in the Statement_Buffer.

   procedure Set_Unit_Name;
   --  Given a unit name in the Name_Buffer, copies it to Statement_Buffer,
   --  starting at the Last + 1 position, and updating last past the value.
   --  changing periods to double underscores, and updating Last appropriately.

   procedure Set_Unit_Number (U : Unit_Id);
   --  Sets unit number (first unit is 1, leading zeroes output to line
   --  up all output unit numbers nicely as required by the value, and
   --  by the total number of units.

   procedure Write_Info_Ada_C (Ada : String; C : String; Common : String);
   --  For C code case, write C & Common, for Ada case write Ada & Common
   --  to current binder output file using Write_Binder_Info.

   procedure Write_Statement_Buffer;
   --  Write out contents of statement buffer up to Last, and reset Last to 0

   procedure Write_Statement_Buffer (S : String);
   --  First writes its argument (using Set_String (S)), then writes out the
   --  contents of statement buffer up to Last, and reset Last to 0

   ------------------------------------
   -- Check_System_Restrictions_Used --
   ------------------------------------

   procedure Check_System_Restrictions_Used is
   begin
      for J in Units.First .. Units.Last loop
         if Get_Name_String (Units.Table (J).Sfile) = "s-restri.ads" then
            System_Restrictions_Used := True;
            return;
         end if;
      end loop;

      System_Restrictions_Used := False;
   end Check_System_Restrictions_Used;

   ----------------------
   -- Gen_Adafinal_Ada --
   ----------------------

   procedure Gen_Adafinal_Ada is
   begin
      WBI ("");
      WBI ("   procedure " & Ada_Final_Name.all & " is");
      WBI ("   begin");

      --  If compiling for the JVM, we directly call Adafinal because
      --  we don't import it via Do_Finalize (see Gen_Output_File_Ada).

      if VM_Target /= No_VM then
         WBI ("      System.Standard_Library.Adafinal;");

      --  If there is no finalization, there is nothing to do

      elsif Cumulative_Restrictions.Set (No_Finalization) then
         WBI ("      null;");
      else
         WBI ("      Do_Finalize;");
      end if;

      WBI ("   end " & Ada_Final_Name.all & ";");
   end Gen_Adafinal_Ada;

   ---------------------
   -- Gen_Adainit_Ada --
   ---------------------

   procedure Gen_Adainit_Ada is
      Main_Priority : Int renames ALIs.Table (ALIs.First).Main_Priority;
      Main_CPU      : Int renames ALIs.Table (ALIs.First).Main_CPU;

   begin
      WBI ("   procedure " & Ada_Init_Name.all & " is");

      --  Generate externals for elaboration entities

      for E in Elab_Order.First .. Elab_Order.Last loop
         declare
            Unum : constant Unit_Id := Elab_Order.Table (E);
            U    : Unit_Record renames Units.Table (Unum);

         begin
            --  Check for Elab_Entity to be set for this unit

            if U.Set_Elab_Entity

            --  Don't generate reference for stand alone library

              and then not U.SAL_Interface

            --  Don't generate reference for predefined file in No_Run_Time
            --  mode, since we don't include the object files in this case

              and then not
                (No_Run_Time_Mode
                   and then Is_Predefined_File_Name (U.Sfile))
            then
               Set_String ("      ");
               Set_String ("E");
               Set_Unit_Number (Unum);

               Set_String (" : Boolean; pragma Import (Ada, ");

               Set_String ("E");
               Set_Unit_Number (Unum);
               Set_String (", """);
               Get_Name_String (U.Uname);

               Set_Unit_Name;
               Set_String ("_E"");");
               Write_Statement_Buffer;
            end if;
         end;
      end loop;

      Write_Statement_Buffer;

      --  Currently in Acton we suppress the standard library. Nothing needs to
      --  be stored in global variables.

      if Suppress_Standard_Library_On_Target then
         WBI ("   begin");
         WBI ("      null;");

      --  Normal case (standard library not suppressed). Set all global values
      --  used by the run time.

      else
         WBI ("      Main_Priority : Integer;");
         WBI ("      pragma Import (C, Main_Priority, " &
              """__gl_main_priority"");");
         WBI ("      Time_Slice_Value : Integer;");
         WBI ("      pragma Import (C, Time_Slice_Value, " &
              """__gl_time_slice_val"");");
         WBI ("      WC_Encoding : Character;");
         WBI ("      pragma Import (C, WC_Encoding, ""__gl_wc_encoding"");");
         WBI ("      Locking_Policy : Character;");
         WBI ("      pragma Import (C, Locking_Policy, " &
              """__gl_locking_policy"");");
         WBI ("      Queuing_Policy : Character;");
         WBI ("      pragma Import (C, Queuing_Policy, " &
              """__gl_queuing_policy"");");
         WBI ("      Task_Dispatching_Policy : Character;");
         WBI ("      pragma Import (C, Task_Dispatching_Policy, " &
              """__gl_task_dispatching_policy"");");
         WBI ("      Priority_Specific_Dispatching : System.Address;");
         WBI ("      pragma Import (C, Priority_Specific_Dispatching, " &
              """__gl_priority_specific_dispatching"");");
         WBI ("      Num_Specific_Dispatching : Integer;");
         WBI ("      pragma Import (C, Num_Specific_Dispatching, " &
              """__gl_num_specific_dispatching"");");
         WBI ("      Main_CPU : Integer;");
         WBI ("      pragma Import (C, Main_CPU, " &
              """__gl_main_cpu"");");

         WBI ("      Interrupt_States : System.Address;");
         WBI ("      pragma Import (C, Interrupt_States, " &
              """__gl_interrupt_states"");");
         WBI ("      Num_Interrupt_States : Integer;");
         WBI ("      pragma Import (C, Num_Interrupt_States, " &
              """__gl_num_interrupt_states"");");
         WBI ("      Unreserve_All_Interrupts : Integer;");
         WBI ("      pragma Import (C, Unreserve_All_Interrupts, " &
              """__gl_unreserve_all_interrupts"");");

         if Exception_Tracebacks then
            WBI ("      Exception_Tracebacks : Integer;");
            WBI ("      pragma Import (C, Exception_Tracebacks, " &
                 """__gl_exception_tracebacks"");");
         end if;

         WBI ("      Zero_Cost_Exceptions : Integer;");
         WBI ("      pragma Import (C, Zero_Cost_Exceptions, " &
              """__gl_zero_cost_exceptions"");");
         WBI ("      Detect_Blocking : Integer;");
         WBI ("      pragma Import (C, Detect_Blocking, " &
              """__gl_detect_blocking"");");
         WBI ("      Default_Stack_Size : Integer;");
         WBI ("      pragma Import (C, Default_Stack_Size, " &
              """__gl_default_stack_size"");");
         WBI ("      Leap_Seconds_Support : Integer;");
         WBI ("      pragma Import (C, Leap_Seconds_Support, " &
              """__gl_leap_seconds_support"");");

         --  Import entry point for elaboration time signal handler
         --  installation, and indication of if it's been called previously.

         WBI ("");
         WBI ("      procedure Install_Handler;");
         WBI ("      pragma Import (C, Install_Handler, " &
              """__gnat_install_handler"");");
         WBI ("");
         WBI ("      Handler_Installed : Integer;");
         WBI ("      pragma Import (C, Handler_Installed, " &
              """__gnat_handler_installed"");");

         --  Import entry point for environment feature enable/disable
         --  routine, and indication that it's been called previously.

         if OpenVMS_On_Target then
            WBI ("");
            WBI ("      procedure Set_Features;");
            WBI ("      pragma Import (C, Set_Features, " &
                 """__gnat_set_features"");");
            WBI ("");
            WBI ("      Features_Set : Integer;");
            WBI ("      pragma Import (C, Features_Set, " &
                 """__gnat_features_set"");");

            if Opt.Heap_Size /= 0 then
               WBI ("");
               WBI ("      Heap_Size : Integer;");
               WBI ("      pragma Import (C, Heap_Size, " &
                    """__gl_heap_size"");");

               Write_Statement_Buffer;
            end if;
         end if;

         --  Initialize stack limit variable of the environment task if the
         --  stack check method is stack limit and stack check is enabled.

         if Stack_Check_Limits_On_Target
           and then (Stack_Check_Default_On_Target or Stack_Check_Switch_Set)
         then
            WBI ("");
            WBI ("      procedure Initialize_Stack_Limit;");
            WBI ("      pragma Import (C, Initialize_Stack_Limit, " &
                 """__gnat_initialize_stack_limit"");");
         end if;

         --  Special processing when main program is CIL function/procedure

         if VM_Target = CLI_Target
           and then Bind_Main_Program
           and then not No_Main_Subprogram
         then
            WBI ("");

            --  Function case, use Set_Exit_Status to report the returned
            --  status code, since that is the only mechanism available.

            if ALIs.Table (ALIs.First).Main_Program = Func then
               WBI ("      Result : Integer;");
               WBI ("      procedure Set_Exit_Status (Code : Integer);");
               WBI ("      pragma Import (C, Set_Exit_Status, " &
                    """__gnat_set_exit_status"");");
               WBI ("");
               WBI ("      function Ada_Main_Program return Integer;");

            --  Procedure case

            else
               WBI ("      procedure Ada_Main_Program;");
            end if;

            Get_Name_String (Units.Table (First_Unit_Entry).Uname);
            Name_Len := Name_Len - 2;
            WBI ("      pragma Import (CIL, Ada_Main_Program, """
                 & Name_Buffer (1 .. Name_Len) & "."
                 & Get_Main_Unit_Name (Name_Buffer (1 .. Name_Len)) & """);");
         end if;

         WBI ("   begin");

         Set_String ("      Main_Priority := ");
         Set_Int    (Main_Priority);
         Set_Char   (';');
         Write_Statement_Buffer;

         Set_String ("      WC_Encoding := '");
         Set_Char   (Get_WC_Encoding);

         Set_String ("';");
         Write_Statement_Buffer;

         Set_String ("      Locking_Policy := '");
         Set_Char   (Locking_Policy_Specified);
         Set_String ("';");
         Write_Statement_Buffer;

         Set_String ("      Queuing_Policy := '");
         Set_Char   (Queuing_Policy_Specified);
         Set_String ("';");
         Write_Statement_Buffer;

         Gen_Restrictions_Ada;

         Set_String ("      Main_CPU := ");
         Set_Int    (Main_CPU);
         Set_Char   (';');
         Write_Statement_Buffer;

         WBI ("      Interrupt_States := Local_Interrupt_States'Address;");

         Set_String ("      Num_Interrupt_States := ");
         Set_Int (IS_Pragma_Settings.Last + 1);
         Set_Char (';');
         Write_Statement_Buffer;

         Set_String ("      Unreserve_All_Interrupts := ");

         if Unreserve_All_Interrupts_Specified then
            Set_String ("1");
         else
            Set_String ("0");
         end if;

         Set_Char (';');
         Write_Statement_Buffer;

         if Exception_Tracebacks then
            WBI ("      Exception_Tracebacks := 1;");
         end if;

         Set_String ("      Zero_Cost_Exceptions := ");

         if Zero_Cost_Exceptions_Specified then
            Set_String ("1");
         else
            Set_String ("0");
         end if;

         Set_String (";");
         Write_Statement_Buffer;

         Set_String ("      Detect_Blocking := ");

         if Detect_Blocking then
            Set_Int (1);
         else
            Set_Int (0);
         end if;

         Set_String (";");
         Write_Statement_Buffer;

         Set_String ("      Default_Stack_Size := ");
         Set_Int (Default_Stack_Size);
         Set_String (";");
         Write_Statement_Buffer;

         Set_String ("      Leap_Seconds_Support := ");

         if Leap_Seconds_Support then
            Set_Int (1);
         else
            Set_Int (0);
         end if;

         Set_String (";");
         Write_Statement_Buffer;

         --  Generate call to Install_Handler

         --  In .NET, when binding with -z, we don't install the signal handler
         --  to let the caller handle the last exception handler.

         if VM_Target /= CLI_Target
           or else Bind_Main_Program
         then
            WBI ("");
            WBI ("      if Handler_Installed = 0 then");
            WBI ("         Install_Handler;");
            WBI ("      end if;");
         end if;

         --  Generate call to Set_Features

         if OpenVMS_On_Target then
            WBI ("");
            WBI ("      if Features_Set = 0 then");
            WBI ("         Set_Features;");
            WBI ("      end if;");

            --  Features_Set may twiddle the heap size according to a logical
            --  name, but the binder switch must override.

            if Opt.Heap_Size /= 0 then
               Set_String ("      Heap_Size := ");
               Set_Int (Opt.Heap_Size);
               Set_Char   (';');
               Write_Statement_Buffer;
            end if;
         end if;
      end if;

      --  Generate call to set Initialize_Scalar values if active

      if Initialize_Scalars_Used then
         WBI ("");
         Set_String ("      System.Scalar_Values.Initialize ('");
         Set_Char (Initialize_Scalars_Mode1);
         Set_String ("', '");
         Set_Char (Initialize_Scalars_Mode2);
         Set_String ("');");
         Write_Statement_Buffer;
      end if;

      --  Generate assignment of default secondary stack size if set

      if Sec_Stack_Used and then Default_Sec_Stack_Size /= -1 then
         WBI ("");
         Set_String ("      System.Secondary_Stack.");
         Set_String ("Default_Secondary_Stack_Size := ");
         Set_Int (Opt.Default_Sec_Stack_Size);
         Set_Char (';');
         Write_Statement_Buffer;
      end if;

      --  Initialize stack limit variable of the environment task if the
      --  stack check method is stack limit and stack check is enabled.

      if Stack_Check_Limits_On_Target
        and then (Stack_Check_Default_On_Target or Stack_Check_Switch_Set)
      then
         WBI ("");
         WBI ("      Initialize_Stack_Limit;");
      end if;

      --  Generate elaboration calls

      WBI ("");
      Gen_Elab_Calls_Ada;

      --  Case of main program is CIL function or procedure

      if VM_Target = CLI_Target
        and then Bind_Main_Program
        and then not No_Main_Subprogram
      then
         --  For function case, use Set_Exit_Status to set result

         if ALIs.Table (ALIs.First).Main_Program = Func then
            WBI ("      Result := Ada_Main_Program;");
            WBI ("      Set_Exit_Status (Result);");

         --  Procedure case

         else
            WBI ("      Ada_Main_Program;");
         end if;
      end if;

      WBI ("   end " & Ada_Init_Name.all & ";");
   end Gen_Adainit_Ada;

   ------------------------
   -- Gen_Elab_Calls_Ada --
   ------------------------

   procedure Gen_Elab_Calls_Ada is
   begin
      for E in Elab_Order.First .. Elab_Order.Last loop
         declare
            Unum : constant Unit_Id := Elab_Order.Table (E);
            U    : Unit_Record renames Units.Table (Unum);

            Unum_Spec : Unit_Id;
            --  This is the unit number of the spec that corresponds to
            --  this entry. It is the same as Unum except when the body
            --  and spec are different and we are currently processing
            --  the body, in which case it is the spec (Unum + 1).

         begin
            if U.Utype = Is_Body then
               Unum_Spec := Unum + 1;
            else
               Unum_Spec := Unum;
            end if;

            --  Nothing to do if predefined unit in no run time mode

            if No_Run_Time_Mode and then Is_Predefined_File_Name (U.Sfile) then
               null;

            --  Case of no elaboration code

            elsif U.No_Elab then

               --  The only case in which we have to do something is if
               --  this is a body, with a separate spec, where the separate
               --  spec has an elaboration entity defined.

               --  In that case, this is where we set the elaboration entity
               --  to True, we do not need to test if this has already been
               --  done, since it is quicker to set the flag than to test it.

               if not U.SAL_Interface and then U.Utype = Is_Body
                 and then Units.Table (Unum_Spec).Set_Elab_Entity
               then
                  Set_String ("      E");
                  Set_Unit_Number (Unum_Spec);
                  Set_String (" := True;");
                  Write_Statement_Buffer;
               end if;

            --  Here if elaboration code is present. If binding a library
            --  or if there is a non-Ada main subprogram then we generate:

            --    if not uname_E then
            --       uname'elab_[spec|body];
            --       uname_E := True;
            --    end if;

            --  Otherwise, elaboration routines are called unconditionally:

            --    uname'elab_[spec|body];
            --    uname_E := True;

            --  The uname_E assignment is skipped if this is a separate spec,
            --  since the assignment will be done when we process the body.

            elsif not U.SAL_Interface then
               if Force_Checking_Of_Elaboration_Flags or
                  Interface_Library_Unit or
                  (not Bind_Main_Program)
               then
                  Set_String ("      if not E");
                  Set_Unit_Number (Unum_Spec);
                  Set_String (" then");
                  Write_Statement_Buffer;
                  Set_String ("   ");
               end if;

               Set_String ("      ");
               Get_Decoded_Name_String_With_Brackets (U.Uname);

               if VM_Target = CLI_Target and then U.Unit_Kind /= 's' then
                  if Name_Buffer (Name_Len) = 's' then
                     Name_Buffer (Name_Len - 1 .. Name_Len + 12) :=
                       "_pkg'elab_spec";
                  else
                     Name_Buffer (Name_Len - 1 .. Name_Len + 12) :=
                       "_pkg'elab_body";
                  end if;

                  Name_Len := Name_Len + 12;

               else
                  if Name_Buffer (Name_Len) = 's' then
                     Name_Buffer (Name_Len - 1 .. Name_Len + 8) :=
                       "'elab_spec";
                  else
                     Name_Buffer (Name_Len - 1 .. Name_Len + 8) :=
                       "'elab_body";
                  end if;

                  Name_Len := Name_Len + 8;
               end if;

               Set_Casing (U.Icasing);
               Set_Name_Buffer;
               Set_Char (';');
               Write_Statement_Buffer;

               if U.Utype /= Is_Spec then
                  if Force_Checking_Of_Elaboration_Flags or
                     Interface_Library_Unit or
                     (not Bind_Main_Program)
                  then
                     Set_String ("   ");
                  end if;

                  Set_String ("      E");
                  Set_Unit_Number (Unum_Spec);
                  Set_String (" := True;");
                  Write_Statement_Buffer;
               end if;

               if Force_Checking_Of_Elaboration_Flags or
                  Interface_Library_Unit or
                  (not Bind_Main_Program)
               then
                  WBI ("      end if;");
               end if;
            end if;
         end;
      end loop;
   end Gen_Elab_Calls_Ada;

   ------------------------
   -- Gen_Elab_Order_Ada --
   ------------------------

   procedure Gen_Elab_Order_Ada is
   begin
      WBI ("");
      WBI ("   --  BEGIN ELABORATION ORDER");

      for J in Elab_Order.First .. Elab_Order.Last loop
         Set_String ("   --  ");
         Get_Name_String (Units.Table (Elab_Order.Table (J)).Uname);
         Set_Name_Buffer;
         Write_Statement_Buffer;
      end loop;

      WBI ("   --  END ELABORATION ORDER");
   end Gen_Elab_Order_Ada;

   ------------------
   -- Gen_Main_Ada --
   ------------------

   procedure Gen_Main_Ada is
   begin
      WBI ("");

      Set_String ("   procedure ");
      Set_String (Get_Main_Name);
      Set_String (" is");
      Write_Statement_Buffer;

      --  Initialize and Finalize

      if not Cumulative_Restrictions.Set (No_Finalization) then
         WBI ("      procedure initialize (Addr : System.Address);");
         WBI ("      pragma Import (C, initialize, ""__gnat_initialize"");");
         WBI ("");
         WBI ("      procedure finalize;");
         WBI ("      pragma Import (C, finalize, ""__gnat_finalize"");");
      end if;

      --  If we want to analyze the stack, we have to import corresponding
      --  symbols

      if Dynamic_Stack_Measurement then
         WBI ("");
         WBI ("      procedure Output_Results;");
         WBI ("      pragma Import (C, Output_Results, " &
              """__gnat_stack_usage_output_results"");");

         WBI ("");
         WBI ("      " &
              "procedure Initialize_Stack_Analysis (Buffer_Size : Natural);");
         WBI ("      pragma Import (C, Initialize_Stack_Analysis, " &
              """__gnat_stack_usage_initialize"");");
      end if;

      --  Deal with declarations for main program case

      if not No_Main_Subprogram then

         --  To call the main program, we declare it using a pragma Import
         --  Ada with the right link name.

         --  It might seem more obvious to "with" the main program, and call
         --  it in the normal Ada manner. We do not do this for three reasons:

         --    1. It is more efficient not to recompile the main program
         --    2. We are not entitled to assume the source is accessible
         --    3. We don't know what options to use to compile it

         --  It is really reason 3 that is most critical (indeed we used
         --  to generate the "with", but several regression tests failed).

         WBI ("");

         WBI ("      procedure Ada_Main_Program;");

         Set_String ("      pragma Import (Ada, Ada_Main_Program, """);
         Get_Name_String (Units.Table (First_Unit_Entry).Uname);
         Set_Main_Program_Name;
         Set_String (""");");

         Write_Statement_Buffer;
         WBI ("");

         if Bind_Main_Program
           and then not Suppress_Standard_Library_On_Target
         then
            WBI ("      SEH : aliased array (1 .. 2) of Integer;");
            WBI ("");
         end if;
      end if;

      --  For the above reasons we call the scheduler agents' initialise
      --  procedure through a pragma Import with the procedure's link name.

      for J in Unique_Dispatching_Policies.First
                 .. Unique_Dispatching_Policies.Last
      loop
         declare
            Policy_Str : String := Get_Name_String
                                (Unique_Dispatching_Policies.Table (J));
         begin
            Set_String ("      procedure Create_Scheduler_Agent_");
            Set_String (Policy_Str);
            Set_String (";");
            Write_Statement_Buffer;

            Set_String ("      pragma Import (Ada, Create_Scheduler_Agent_");
            Set_String (Policy_Str);
            Set_String (", ""__acton_scheduler_agent_");
            Set_String (Policy_Str);
            Set_String (""");");
            Write_Statement_Buffer;
         end;

         WBI ("");
      end loop;

      --  Generate a reference to Ada_Main_Program_Name. This symbol is
      --  not referenced elsewhere in the generated program, but is needed
      --  by the debugger (that's why it is generated in the first place).
      --  The reference stops Ada_Main_Program_Name from being optimized
      --  away by smart linkers, such as the AiX linker.

      --  Because this variable is unused, we make this variable "aliased"
      --  with a pragma Volatile in order to tell the compiler to preserve
      --  this variable at any level of optimization.

      if Bind_Main_Program then
         WBI
           ("      Ensure_Reference : aliased System.Address := " &
            "Ada_Main_Program_Name'Address;");
         WBI ("      pragma Volatile (Ensure_Reference);");
         WBI ("");
      end if;

      --  Generate OTCR for the main task.

      WBI ("      Main_Task_OTCR : aliased Oak.Oak_Task.Oak_Task;");

      --  And now the OTCRs for the Scheduler Agents.

      for J in Scheduler_Agents.First .. Scheduler_Agents.Last loop
         Set_String ("      Scheduler_Agent_");
         Set_Int (Int (J));
         Set_String (" : aliased Oak.Oak_Task.Oak_Task;");
         Write_Statement_Buffer;
      end loop;

      WBI ("");

      WBI ("   begin");

      if Dynamic_Stack_Measurement then
         Set_String ("      Initialize_Stack_Analysis (");
         Set_Int (Dynamic_Stack_Measurement_Array_Size);
         Set_String (");");
         Write_Statement_Buffer;
      end if;

      if not Cumulative_Restrictions.Set (No_Finalization) then
         if not No_Main_Subprogram
           and then Bind_Main_Program
           and then not Suppress_Standard_Library_On_Target
         then
            WBI ("      Initialize (SEH'Address);");
         else
            WBI ("      Initialize (System.Null_Address);");
         end if;
      end if;

      WBI ("      " & Ada_Init_Name.all & ";");

      if not No_Main_Subprogram then
         WBI ("      Break_Start;");
         WBI ("");

         for J in Scheduler_Agents.First .. Scheduler_Agents.Last loop
            Set_String ("      Create_Scheduler_Agent_");
            Set_String (Get_Name_String
                          (Scheduler_Agents.Table (J).Dispatching_Policy));
            Set_String (" (Scheduler_Agent_");
            Set_Int (Int (J));
            Set_String ("'Access, ");
            Set_Int (Scheduler_Agents.Table (J).First_Priority);
            Set_String (", ");
            Set_Int (Scheduler_Agents.Table (J).Last_Priority);
            Set_String (");");
            Write_Statement_Buffer;
         end loop;

         --  Initalise main task call
         WBI ("      Initialise_Main_Task");
         WBI ("        (Main_Task_OTCR'Access,");

         --  Set the stack size of the main task
         if ALIs.Table (ALIs.First).Main_Stack_Size = No_Main_Stack_Size then
            WBI ("         Oak.Processor_Support_Package." &
                 "Call_Stack.Main_Task_Call_Stack_Size,");
         else
            Set_String ("         ");
            Set_Int (ALIs.Table (ALIs.First).Main_Stack_Size);
            Set_String (", ");
            Write_Statement_Buffer;
         end if;

         --  Set the name of the main task.
         WBI ("         Main Task,");

         --  Set the priority of the main task
         if ALIs.Table (ALIs.First).Main_Priority = No_Main_Priority then
            WBI ("         System.Default_Priority,");
         else
            Set_String ("         ");
            Set_Int (ALIs.Table (ALIs.First).Main_Priority);
            Set_String (",");
            Write_Statement_Buffer;
         end if;

         --  Set the relative deadline of the main task.
         Set_String ("         ");
         Set_Int (ALIs.Table (ALIs.First).Main_Deadline);
         Set_String (",");
         Write_Statement_Buffer;

         --  Set Cycle_Period
         Set_String ("         ");
         Set_Int (ALIs.Table (ALIs.First).Main_Cycle_Period);
         Set_String (",");
         Write_Statement_Buffer;

         --  Set the Address of the main task's run-loop
         WBI ("         Ada_Main_Program'Address);");
      end if;

      --  Adafinal call is skipped if no finalization

      if not Cumulative_Restrictions.Set (No_Finalization) then

         --  If compiling for the JVM, we directly call Adafinal because
         --  we don't import it via Do_Finalize (see Gen_Output_File_Ada).

         if VM_Target = No_VM then
            WBI ("      Do_Finalize;");
         else
            WBI ("      System.Standard_Library.Adafinal;");
         end if;
      end if;

      --  Prints the result of static stack analysis

      if Dynamic_Stack_Measurement then
         WBI ("      Output_Results;");
      end if;

      --  Finalize is only called if we have a run time

      if not Cumulative_Restrictions.Set (No_Finalization) then
         WBI ("      Finalize;");
      end if;

      WBI ("   end;");
   end Gen_Main_Ada;

   ------------------------------
   -- Gen_Object_Files_Options --
   ------------------------------

   procedure Gen_Object_Files_Options is
      Lgnat : Natural;
      --  This keeps track of the position in the sorted set of entries
      --  in the Linker_Options table of where the first entry from an
      --  internal file appears.

      Linker_Option_List_Started : Boolean := False;
      --  Set to True when "LINKER OPTION LIST" is displayed

      procedure Write_Linker_Option;
      --  Write binder info linker option

      -------------------------
      -- Write_Linker_Option --
      -------------------------

      procedure Write_Linker_Option is
         Start : Natural;
         Stop  : Natural;

      begin
         --  Loop through string, breaking at null's

         Start := 1;
         while Start < Name_Len loop

            --  Find null ending this section

            Stop := Start + 1;
            while Name_Buffer (Stop) /= ASCII.NUL
              and then Stop <= Name_Len loop
               Stop := Stop + 1;
            end loop;

            --  Process section if non-null

            if Stop > Start then
               if Output_Linker_Option_List then
                  if not Zero_Formatting then
                     if not Linker_Option_List_Started then
                        Linker_Option_List_Started := True;
                        Write_Eol;
                        Write_Str ("     LINKER OPTION LIST");
                        Write_Eol;
                        Write_Eol;
                     end if;

                     Write_Str ("   ");
                  end if;

                  Write_Str (Name_Buffer (Start .. Stop - 1));
                  Write_Eol;
               end if;
               Write_Info_Ada_C
                 ("   --   ", "", Name_Buffer (Start .. Stop - 1));
            end if;

            Start := Stop + 1;
         end loop;
      end Write_Linker_Option;

   --  Start of processing for Gen_Object_Files_Options

   begin
      WBI ("");
      Write_Info_Ada_C ("-- ", "/* ", " BEGIN Object file/option list");

      if Object_List_Filename /= null then
         Set_List_File (Object_List_Filename.all);
      end if;

      for E in Elab_Order.First .. Elab_Order.Last loop

         --  If not spec that has an associated body, then generate a comment
         --  giving the name of the corresponding object file.

         if (not Units.Table (Elab_Order.Table (E)).SAL_Interface)
           and then Units.Table (Elab_Order.Table (E)).Utype /= Is_Spec
         then
            Get_Name_String
              (ALIs.Table
                (Units.Table (Elab_Order.Table (E)).My_ALI).Ofile_Full_Name);

            --  If the presence of an object file is necessary or if it exists,
            --  then use it.

            if not Hostparm.Exclude_Missing_Objects
              or else
                System.OS_Lib.Is_Regular_File (Name_Buffer (1 .. Name_Len))
            then
               Write_Info_Ada_C ("   --   ", "", Name_Buffer (1 .. Name_Len));

               if Output_Object_List then
                  Write_Str (Name_Buffer (1 .. Name_Len));
                  Write_Eol;
               end if;

               --  Don't link with the shared library on VMS if an internal
               --  filename object is seen. Multiply defined symbols will
               --  result.

               if OpenVMS_On_Target
                 and then Is_Internal_File_Name
                  (ALIs.Table
                   (Units.Table (Elab_Order.Table (E)).My_ALI).Sfile)
               then
                  --  Special case for g-trasym.obj (not included in libgnat)

                  Get_Name_String (ALIs.Table
                            (Units.Table (Elab_Order.Table (E)).My_ALI).Sfile);

                  if Name_Buffer (1 .. 8) /= "g-trasym" then
                     Opt.Shared_Libgnat := False;
                  end if;
               end if;
            end if;
         end if;
      end loop;

      if Object_List_Filename /= null then
         Close_List_File;
      end if;

      --  Add a "-Ldir" for each directory in the object path
      if VM_Target /= CLI_Target then
         for J in 1 .. Nb_Dir_In_Obj_Search_Path loop
            declare
               Dir : constant String_Ptr := Dir_In_Obj_Search_Path (J);
            begin
               Name_Len := 0;
               Add_Str_To_Name_Buffer ("-L");
               Add_Str_To_Name_Buffer (Dir.all);
               Write_Linker_Option;
            end;
         end loop;
      end if;

      --  Sort linker options

      --  This sort accomplishes two important purposes:

      --    a) All application files are sorted to the front, and all GNAT
      --       internal files are sorted to the end. This results in a well
      --       defined dividing line between the two sets of files, for the
      --       purpose of inserting certain standard library references into
      --       the linker arguments list.

      --    b) Given two different units, we sort the linker options so that
      --       those from a unit earlier in the elaboration order comes later
      --       in the list. This is a heuristic designed to create a more
      --       friendly order of linker options when the operations appear in
      --       separate units. The idea is that if unit A must be elaborated
      --       before unit B, then it is more likely that B references
      --       libraries included by A, than vice versa, so we want libraries
      --       included by A to come after libraries included by B.

      --  These two criteria are implemented by function Lt_Linker_Option. Note
      --  that a special case of b) is that specs are elaborated before bodies,
      --  so linker options from specs come after linker options for bodies,
      --  and again, the assumption is that libraries used by the body are more
      --  likely to reference libraries used by the spec, than vice versa.

      Sort
        (Linker_Options.Last,
         Move_Linker_Option'Access,
         Lt_Linker_Option'Access);

      --  Write user linker options, i.e. the set of linker options that come
      --  from all files other than GNAT internal files, Lgnat is left set to
      --  point to the first entry from a GNAT internal file, or past the end
      --  of the entries if there are no internal files.

      Lgnat := Linker_Options.Last + 1;

      for J in 1 .. Linker_Options.Last loop
         if not Linker_Options.Table (J).Internal_File then
            Get_Name_String (Linker_Options.Table (J).Name);
            Write_Linker_Option;
         else
            Lgnat := J;
            exit;
         end if;
      end loop;

      --  Now we insert standard linker options that must appear after the
      --  entries from user files, and before the entries from GNAT run-time
      --  files. The reason for this decision is that libraries referenced
      --  by internal routines may reference these standard library entries.

      --  Note that we do not insert anything when pragma No_Run_Time has been
      --  specified or when the standard libraries are not to be used,
      --  otherwise on some platforms, such as VMS, we may get duplicate
      --  symbols when linking.

      if not (Opt.No_Run_Time_Mode or else Opt.No_Stdlib) then
         Name_Len := 0;

         if Opt.Shared_Libgnat then
            Add_Str_To_Name_Buffer ("-shared");
         else
            Add_Str_To_Name_Buffer ("-static");
         end if;

         --  Write directly to avoid -K output (why???)

         Write_Info_Ada_C ("   --   ", "", Name_Buffer (1 .. Name_Len));

         if With_DECGNAT then
            Name_Len := 0;

            if Opt.Shared_Libgnat then
               Add_Str_To_Name_Buffer (Shared_Lib ("decgnat"));
            else
               Add_Str_To_Name_Buffer ("-ldecgnat");
            end if;

            Write_Linker_Option;
         end if;

         if With_GNARL then
            Name_Len := 0;

            if Opt.Shared_Libgnat then
               Add_Str_To_Name_Buffer (Shared_Lib ("gnarl"));
            else
               Add_Str_To_Name_Buffer ("-lgnarl");
            end if;

            Write_Linker_Option;
         end if;

         Name_Len := 0;

         if Opt.Shared_Libgnat then
            Add_Str_To_Name_Buffer (Shared_Lib ("gnat"));
         else
            Add_Str_To_Name_Buffer ("-lgnat");
         end if;

         Write_Linker_Option;
      end if;

      --  Write linker options from all internal files

      for J in Lgnat .. Linker_Options.Last loop
         Get_Name_String (Linker_Options.Table (J).Name);
         Write_Linker_Option;
      end loop;

      if Output_Linker_Option_List and then not Zero_Formatting then
         Write_Eol;
      end if;

      if Ada_Bind_File then
         WBI ("--  END Object file/option list   ");
      else
         WBI ("    END Object file/option list */");
      end if;
   end Gen_Object_Files_Options;

   ---------------------
   -- Gen_Output_File --
   ---------------------

   procedure Gen_Output_File (Filename : String) is
   begin
      --  Acquire settings for Interrupt_State pragmas

      Set_IS_Pragma_Table;

      --  Acquire Scheduler Agents

      Set_Scheduler_Agent_Table;

      --  Override Ada_Bind_File and Bind_Main_Program for VMs since JGNAT only
      --  supports Ada code, and the main program is already generated by the
      --  compiler.

      if VM_Target /= No_VM then
         Ada_Bind_File := True;

         if VM_Target = JVM_Target then
            Bind_Main_Program := False;
         end if;
      end if;

      --  Override time slice value if -T switch is set

      if Time_Slice_Set then
         ALIs.Table (ALIs.First).Time_Slice_Value := Opt.Time_Slice_Value;
      end if;

      --  Count number of elaboration calls

      for E in Elab_Order.First .. Elab_Order.Last loop
         if Units.Table (Elab_Order.Table (E)).No_Elab then
            null;
         else
            Num_Elab_Calls := Num_Elab_Calls + 1;
         end if;
      end loop;

      --  Generate output file in appropriate language

      Check_System_Restrictions_Used;

      Gen_Output_File_Ada (Filename);
   end Gen_Output_File;

   -------------------------
   -- Gen_Output_File_Ada --
   -------------------------

   procedure Gen_Output_File_Ada (Filename : String) is

      Bfiles : Name_Id;
      --  Name of generated bind file (spec)

      Bfileb : Name_Id;
      --  Name of generated bind file (body)

      Ada_Main : constant String := Get_Ada_Main_Name;
      --  Name to be used for generated Ada main program. See the body of
      --  function Get_Ada_Main_Name for details on the form of the name.

   begin
      --  Create spec first

      Create_Binder_Output (Filename, 's', Bfiles);

      --  We always compile the binder file in Ada 95 mode so that we properly
      --  handle use of Ada 2005 keywords as identifiers in Ada 95 mode. None
      --  of the Ada 2005 constructs are needed by the binder file.

      WBI ("pragma Ada_95;");

      --  If we are operating in Restrictions (No_Exception_Handlers) mode,
      --  then we need to make sure that the binder program is compiled with
      --  the same restriction, so that no exception tables are generated.

      if Cumulative_Restrictions.Set (No_Exception_Handlers) then
         WBI ("pragma Restrictions (No_Exception_Handlers);");
      end if;

      --  Same processing for Restrictions (No_Exception_Propagation)

      if Cumulative_Restrictions.Set (No_Exception_Propagation) then
         WBI ("pragma Restrictions (No_Exception_Propagation);");
      end if;

      --  Same processing for pragma No_Run_Time

      if No_Run_Time_Mode then
         WBI ("pragma No_Run_Time;");
      end if;

      --  Generate with of System so we can reference System.Address

      WBI ("with System;");

      --  Generate with of System.Initialize_Scalars if active

      if Initialize_Scalars_Used then
         WBI ("with System.Scalar_Values;");
      end if;

      --  Generate with of System.Secondary_Stack if active

      if Sec_Stack_Used and then Default_Sec_Stack_Size /= -1 then
         WBI ("with System.Secondary_Stack;");
      end if;

      Resolve_Binder_Options;

      if VM_Target /= No_VM then
         if not Suppress_Standard_Library_On_Target then

            --  Usually, adafinal is called using a pragma Import C. Since
            --  Import C doesn't have the same semantics for JGNAT, we use
            --  standard Ada.

            WBI ("with System.Standard_Library;");
         end if;
      end if;

      --  Generate with of Oak.Oak_Task so that we can reference
      --  Oak.Oak_Task.Oak_Task to create the main task and the scheduler
      --  agents OTCR.

      WBI ("with Oak.Oak_Task;");

      WBI ("package " & Ada_Main & " is");
      WBI ("   pragma Warnings (Off);");

      --  Main program case

      if Bind_Main_Program then

         --  Generate the GNAT_Version and Ada_Main_Program_Name info only for
         --  the main program. Otherwise, it can lead under some circumstances
         --  to a symbol duplication during the link (for instance when a C
         --  program uses two Ada libraries). Also zero terminate the string
         --  so that its end can be found reliably at run time.

         WBI ("");
         WBI ("   GNAT_Version : constant String :=");
         WBI ("                    """ & Ver_Prefix &
                                   Gnat_Version_String &
                                   """ & ASCII.NUL;");
         WBI ("   pragma Export (C, GNAT_Version, ""__gnat_version"");");

         WBI ("");
         Set_String ("   Ada_Main_Program_Name : constant String := """);
         Get_Name_String (Units.Table (First_Unit_Entry).Uname);

         Set_Main_Program_Name;
         Set_String (""" & ASCII.NUL;");
         Write_Statement_Buffer;

         WBI
           ("   pragma Export (C, Ada_Main_Program_Name, " &
            """__gnat_ada_main_program_name"");");
      end if;

      if not Cumulative_Restrictions.Set (No_Finalization) then
         WBI ("");
         WBI ("   procedure " & Ada_Final_Name.all & ";");
         WBI ("   pragma Export (C, " & Ada_Final_Name.all & ", """ &
              Ada_Final_Name.all & """);");
      end if;

      WBI ("");
      WBI ("   procedure " & Ada_Init_Name.all & ";");
      WBI ("   pragma Export (C, " & Ada_Init_Name.all & ", """ &
           Ada_Init_Name.all & """);");

      --  If -a has been specified use pragma Linker_Constructor for the init
      --  procedure. No need to use a similar pragma for the final procedure as
      --  global finalization will occur when the executable finishes execution
      --  and for plugins (shared stand-alone libraries that can be
      --  "unloaded"), finalization should not occur automatically, otherwise
      --  the main executable may not continue to work properly.

      if Use_Pragma_Linker_Constructor then
         WBI ("   pragma Linker_Constructor (" & Ada_Init_Name.all & ");");
      end if;

      if Bind_Main_Program and then VM_Target = No_VM then

         --  If we have the standard library, then Break_Start is defined
         --  there, but when the standard library is suppressed, Break_Start
         --  is defined here.

         WBI ("");
         WBI ("   procedure Break_Start;");

         if Suppress_Standard_Library_On_Target then
            WBI ("   pragma Export (C, Break_Start, ""__gnat_break_start"");");
         else
            WBI ("   pragma Import (C, Break_Start, ""__gnat_break_start"");");
         end if;

         WBI ("");

         Set_String ("   procedure ");
         Set_String (Get_Main_Name);
         Write_Statement_Buffer (";");

         WBI ("   pragma Export (C, " & Get_Main_Name & ", """ &
           Get_Main_Name & """);");
      end if;

      Gen_Versions_Ada;
      Gen_Elab_Order_Ada;

      --  Spec is complete

      WBI ("");
      WBI ("end " & Ada_Main & ";");
      Close_Binder_Output;

      --
      --  Prepare to write body
      --

      Create_Binder_Output (Filename, 'b', Bfileb);

      --  We always compile the binder file in Ada 95 mode so that we properly
      --  handle use of Ada 2005 keywords as identifiers in Ada 95 mode. None
      --  of the Ada 2005 constructs are needed by the binder file.

      WBI ("pragma Ada_95;");

      --  Output Source_File_Name pragmas which look like

      --    pragma Source_File_Name (Ada_Main, Spec_File_Name => "sss");
      --    pragma Source_File_Name (Ada_Main, Body_File_Name => "bbb");

      --  where sss/bbb are the spec/body file names respectively

      Get_Name_String (Bfiles);
      Name_Buffer (Name_Len + 1 .. Name_Len + 3) := """);";

      WBI ("pragma Source_File_Name (" &
           Ada_Main &
           ", Spec_File_Name => """ &
           Name_Buffer (1 .. Name_Len + 3));

      Get_Name_String (Bfileb);
      Name_Buffer (Name_Len + 1 .. Name_Len + 3) := """);";

      WBI ("pragma Source_File_Name (" &
           Ada_Main &
           ", Body_File_Name => """ &
           Name_Buffer (1 .. Name_Len + 3));

      --  Generate with of System.Restrictions to initialize
      --  Run_Time_Restrictions.

      if System_Restrictions_Used
        and not Suppress_Standard_Library_On_Target
      then
         WBI ("");
         WBI ("with System.Restrictions;");
      end if;

      WBI ("");
      WBI ("package body " & Ada_Main & " is");
      WBI ("   pragma Warnings (Off);");

      --  Import the finalization procedure only if finalization active

      if not Cumulative_Restrictions.Set (No_Finalization) then

         --  In the Java case, pragma Import C cannot be used, so the standard
         --  Ada constructs will be used instead.

         if VM_Target = No_VM then
            WBI ("");
            WBI ("   procedure Do_Finalize;");
            WBI
              ("   pragma Import (C, Do_Finalize, " &
               """system__standard_library__adafinal"");");
            WBI ("");
         end if;
      end if;

      Gen_Adainit_Ada;

      --  Generate the adafinal routine unless there is no finalization to do

      if not Cumulative_Restrictions.Set (No_Finalization) then
         Gen_Adafinal_Ada;
      end if;

      if Bind_Main_Program and then VM_Target = No_VM then

         --  When suppressing the standard library then generate dummy body
         --  for Break_Start

         if Suppress_Standard_Library_On_Target then
            WBI ("");
            WBI ("   procedure Break_Start is");
            WBI ("   begin");
            WBI ("      null;");
            WBI ("   end;");
         end if;

         Gen_Main_Ada;
      end if;

      --  Output object file list and the Ada body is complete

      Gen_Object_Files_Options;

      WBI ("");
      WBI ("end " & Ada_Main & ";");

      Close_Binder_Output;
   end Gen_Output_File_Ada;

   --------------------------
   -- Gen_Restrictions_Ada --
   --------------------------

   procedure Gen_Restrictions_Ada is
      Count : Integer;

   begin
      if Suppress_Standard_Library_On_Target
        or not System_Restrictions_Used
      then
         return;
      end if;

      WBI ("      System.Restrictions.Run_Time_Restrictions :=");
      WBI ("        (Set =>");
      Set_String      ("          (");

      Count := 0;

      for J in Cumulative_Restrictions.Set'Range loop
         Set_Boolean (Cumulative_Restrictions.Set (J));
         Set_String (", ");
         Count := Count + 1;

         if J /= Cumulative_Restrictions.Set'Last and then Count = 8 then
            Write_Statement_Buffer;
            Set_String ("           ");
            Count := 0;
         end if;
      end loop;

      Set_String_Replace ("),");
      Write_Statement_Buffer;
      Set_String ("         Value => (");

      for J in Cumulative_Restrictions.Value'Range loop
         Set_Int (Int (Cumulative_Restrictions.Value (J)));
         Set_String (", ");
      end loop;

      Set_String_Replace ("),");
      Write_Statement_Buffer;
      WBI ("         Violated =>");
      Set_String ("          (");
      Count := 0;

      for J in Cumulative_Restrictions.Violated'Range loop
         Set_Boolean (Cumulative_Restrictions.Violated (J));
         Set_String (", ");
         Count := Count + 1;

         if J /= Cumulative_Restrictions.Set'Last and then Count = 8 then
            Write_Statement_Buffer;
            Set_String ("           ");
            Count := 0;
         end if;
      end loop;

      Set_String_Replace ("),");
      Write_Statement_Buffer;
      Set_String ("         Count => (");

      for J in Cumulative_Restrictions.Count'Range loop
         Set_Int (Int (Cumulative_Restrictions.Count (J)));
         Set_String (", ");
      end loop;

      Set_String_Replace ("),");
      Write_Statement_Buffer;
      Set_String ("         Unknown => (");

      for J in Cumulative_Restrictions.Unknown'Range loop
         Set_Boolean (Cumulative_Restrictions.Unknown (J));
         Set_String (", ");
      end loop;

      Set_String_Replace ("))");
      Set_String (";");
      Write_Statement_Buffer;
   end Gen_Restrictions_Ada;

   ----------------------
   -- Gen_Versions_Ada --
   ----------------------

   --  This routine generates lines such as:

   --    unnnnn : constant Integer := 16#hhhhhhhh#;
   --    pragma Export (C, unnnnn, unam);

   --  for each unit, where unam is the unit name suffixed by either B or S for
   --  body or spec, with dots replaced by double underscores, and hhhhhhhh is
   --  the version number, and nnnnn is a 5-digits serial number.

   procedure Gen_Versions_Ada is
      Ubuf : String (1 .. 6) := "u00000";

      procedure Increment_Ubuf;
      --  Little procedure to increment the serial number

      procedure Increment_Ubuf is
      begin
         for J in reverse Ubuf'Range loop
            Ubuf (J) := Character'Succ (Ubuf (J));
            exit when Ubuf (J) <= '9';
            Ubuf (J) := '0';
         end loop;
      end Increment_Ubuf;

   --  Start of processing for Gen_Versions_Ada

   begin
      WBI ("");

      WBI ("   type Version_32 is mod 2 ** 32;");
      for U in Units.First .. Units.Last loop
         if not Units.Table (U).SAL_Interface and then
           ((not Bind_For_Library) or else Units.Table (U).Directly_Scanned)
         then
            Increment_Ubuf;
            WBI ("   " & Ubuf & " : constant Version_32 := 16#" &
                 Units.Table (U).Version & "#;");
            Set_String ("   pragma Export (C, ");
            Set_String (Ubuf);
            Set_String (", """);

            Get_Name_String (Units.Table (U).Uname);

            for K in 1 .. Name_Len loop
               if Name_Buffer (K) = '.' then
                  Set_Char ('_');
                  Set_Char ('_');

               elsif Name_Buffer (K) = '%' then
                  exit;

               else
                  Set_Char (Name_Buffer (K));
               end if;
            end loop;

            if Name_Buffer (Name_Len) = 's' then
               Set_Char ('S');
            else
               Set_Char ('B');
            end if;

            Set_String (""");");
            Write_Statement_Buffer;
         end if;
      end loop;

   end Gen_Versions_Ada;

   ------------------------
   -- Get_Main_Unit_Name --
   ------------------------

   function Get_Main_Unit_Name (S : String) return String is
      Result : String := S;

   begin
      for J in S'Range loop
         if Result (J) = '.' then
            Result (J) := '_';
         end if;
      end loop;

      return Result;
   end Get_Main_Unit_Name;

   -----------------------
   -- Get_Ada_Main_Name --
   -----------------------

   function Get_Ada_Main_Name return String is
      Suffix : constant String := "_00";
      Name   : String (1 .. Opt.Ada_Main_Name.all'Length + Suffix'Length) :=
                 Opt.Ada_Main_Name.all & Suffix;
      Nlen   : Natural;

   begin
      --  The main program generated by JGNAT expects a package called
      --  ada_<main procedure>.

      if VM_Target /= No_VM then
         Get_Name_String (Units.Table (First_Unit_Entry).Uname);
         return "ada_" & Get_Main_Unit_Name (Name_Buffer (1 .. Name_Len - 2));
      end if;

      --  This loop tries the following possibilities in order
      --    <Ada_Main>
      --    <Ada_Main>_01
      --    <Ada_Main>_02
      --    ..
      --    <Ada_Main>_99
      --  where <Ada_Main> is equal to Opt.Ada_Main_Name. By default,
      --  it is set to 'ada_main'.

      for J in 0 .. 99 loop
         if J = 0 then
            Nlen := Name'Length - Suffix'Length;
         else
            Nlen := Name'Length;
            Name (Name'Last) := Character'Val (J mod 10 + Character'Pos ('0'));
            Name (Name'Last - 1) :=
              Character'Val (J /   10 + Character'Pos ('0'));
         end if;

         for K in ALIs.First .. ALIs.Last loop
            for L in ALIs.Table (K).First_Unit .. ALIs.Table (K).Last_Unit loop

               --  Get unit name, removing %b or %e at end

               Get_Name_String (Units.Table (L).Uname);
               Name_Len := Name_Len - 2;

               if Name_Buffer (1 .. Name_Len) = Name (1 .. Nlen) then
                  goto Continue;
               end if;
            end loop;
         end loop;

         return Name (1 .. Nlen);

      <<Continue>>
         null;
      end loop;

      --  If we fall through, just use a peculiar unlikely name

      return ("Qwertyuiop");
   end Get_Ada_Main_Name;

   -------------------
   -- Get_Main_Name --
   -------------------

   function Get_Main_Name return String is
   begin
      --  Explicit name given with -M switch

      if Bind_Alternate_Main_Name then
         return Alternate_Main_Name.all;

      --  Case of main program name to be used directly

      elsif Use_Ada_Main_Program_Name_On_Target then

         --  Get main program name

         Get_Name_String (Units.Table (First_Unit_Entry).Uname);

         --  If this is a child name, return only the name of the child, since
         --  we can't have dots in a nested program name. Note that we do not
         --  include the %b at the end of the unit name.

         for J in reverse 1 .. Name_Len - 2 loop
            if J = 1 or else Name_Buffer (J - 1) = '.' then
               return Name_Buffer (J .. Name_Len - 2);
            end if;
         end loop;

         raise Program_Error; -- impossible exit

      --  Case where "main" is to be used as default

      else
         return "main";
      end if;
   end Get_Main_Name;

   ---------------------
   -- Get_WC_Encoding --
   ---------------------

   function Get_WC_Encoding return Character is
   begin
      --  If encoding method specified by -W switch, then return it

      if Wide_Character_Encoding_Method_Specified then
         return WC_Encoding_Letters (Wide_Character_Encoding_Method);

      --  If no main program, and not specified, set brackets, we really have
      --  no better choice. If some other encoding is required when there is
      --  no main, it must be set explicitly using -Wx.

      --  Note: if the ALI file always passed the wide character encoding of
      --  every file, then we could use the encoding of the initial specified
      --  file, but this information is passed only for potential main
      --  programs. We could fix this sometime, but it is a very minor point
      --  (wide character default encoding for [Wide_[Wide_]Text_IO when there
      --  is no main program).

      elsif No_Main_Subprogram then
         return 'b';

      --  Otherwise if there is a main program, take encoding from it

      else
         return ALIs.Table (ALIs.First).WC_Encoding;
      end if;
   end Get_WC_Encoding;

   ----------------------
   -- Lt_Linker_Option --
   ----------------------

   function Lt_Linker_Option (Op1, Op2 : Natural) return Boolean is
   begin
      --  Sort internal files last

      if Linker_Options.Table (Op1).Internal_File
           /=
         Linker_Options.Table (Op2).Internal_File
      then
         --  Note: following test uses False < True

         return Linker_Options.Table (Op1).Internal_File
                  <
                Linker_Options.Table (Op2).Internal_File;

      --  If both internal or both non-internal, sort according to the
      --  elaboration position. A unit that is elaborated later should come
      --  earlier in the linker options list.

      else
         return Units.Table (Linker_Options.Table (Op1).Unit).Elab_Position
                  >
                Units.Table (Linker_Options.Table (Op2).Unit).Elab_Position;

      end if;
   end Lt_Linker_Option;

   ------------------------
   -- Move_Linker_Option --
   ------------------------

   procedure Move_Linker_Option (From : Natural; To : Natural) is
   begin
      Linker_Options.Table (To) := Linker_Options.Table (From);
   end Move_Linker_Option;

   ----------------------------
   -- Resolve_Binder_Options --
   ----------------------------

   procedure Resolve_Binder_Options is
   begin
      for E in Elab_Order.First .. Elab_Order.Last loop
         Get_Name_String (Units.Table (Elab_Order.Table (E)).Uname);

         --  This is not a perfect approach, but is the current protocol
         --  between the run-time and the binder to indicate that tasking is
         --  used: system.os_interface should always be used by any tasking
         --  application.

         if Name_Buffer (1 .. 19) = "system.os_interface" then
            With_GNARL := True;
         end if;

         --  Ditto for declib and the "dec" package

         if OpenVMS_On_Target and then Name_Buffer (1 .. 5) = "dec%s" then
            With_DECGNAT := True;
         end if;
      end loop;
   end Resolve_Binder_Options;

   -----------------
   -- Set_Boolean --
   -----------------

   procedure Set_Boolean (B : Boolean) is
      True_Str  : constant String := "True";
      False_Str : constant String := "False";
   begin
      if B then
         Statement_Buffer (Last + 1 .. Last + True_Str'Length) := True_Str;
         Last := Last + True_Str'Length;
      else
         Statement_Buffer (Last + 1 .. Last + False_Str'Length) := False_Str;
         Last := Last + False_Str'Length;
      end if;
   end Set_Boolean;

   --------------
   -- Set_Char --
   --------------

   procedure Set_Char (C : Character) is
   begin
      Last := Last + 1;
      Statement_Buffer (Last) := C;
   end Set_Char;

   -------------
   -- Set_Int --
   -------------

   procedure Set_Int (N : Int) is
   begin
      if N < 0 then
         Set_String ("-");
         Set_Int (-N);

      else
         if N > 9 then
            Set_Int (N / 10);
         end if;

         Last := Last + 1;
         Statement_Buffer (Last) :=
           Character'Val (N mod 10 + Character'Pos ('0'));
      end if;
   end Set_Int;

   -------------------------
   -- Set_IS_Pragma_Table --
   -------------------------

   procedure Set_IS_Pragma_Table is
   begin
      for F in ALIs.First .. ALIs.Last loop
         for K in ALIs.Table (F).First_Interrupt_State ..
                  ALIs.Table (F).Last_Interrupt_State
         loop
            declare
               Inum : constant Int :=
                        Interrupt_States.Table (K).Interrupt_Id;
               Stat : constant Character :=
                        Interrupt_States.Table (K).Interrupt_State;

            begin
               while IS_Pragma_Settings.Last < Inum loop
                  IS_Pragma_Settings.Append ('n');
               end loop;

               IS_Pragma_Settings.Table (Inum) := Stat;
            end;
         end loop;
      end loop;
   end Set_IS_Pragma_Table;

   ---------------------------
   -- Set_Main_Program_Name --
   ---------------------------

   procedure Set_Main_Program_Name is
   begin
      --  Note that name has %b on the end which we ignore

      --  First we output the initial _ada_ since we know that the main
      --  program is a library level subprogram.

      Set_String ("_ada_");

      --  Copy name, changing dots to double underscores

      for J in 1 .. Name_Len - 2 loop
         if Name_Buffer (J) = '.' then
            Set_String ("__");
         else
            Set_Char (Name_Buffer (J));
         end if;
      end loop;
   end Set_Main_Program_Name;

   ---------------------
   -- Set_Name_Buffer --
   ---------------------

   procedure Set_Name_Buffer is
   begin
      for J in 1 .. Name_Len loop
         Set_Char (Name_Buffer (J));
      end loop;
   end Set_Name_Buffer;

   -------------------------------
   -- Set_Scheduler_Agent_Table --
   -------------------------------

   procedure Set_Scheduler_Agent_Table is
      Priority_Table : array (Int (System.Any_Priority'First) ..
                              Int (System.Any_Priority'Last)) of
                         Name_Id :=
                           (others => Unique_Dispatching_Policies.Table
                             (Unique_Dispatching_Policies.First));
      --  Array containing an entry per priority consisting of Name_Id

      Policy         : Name_Id;
      First_Priority : Nat;
      SDR            : Specific_Dispatching_Record;

   begin
      for J in Specific_Dispatching.First .. Specific_Dispatching.Last loop
         declare
            SDTJ : Specific_Dispatching_Record
                     renames Specific_Dispatching.Table (J);
         begin
            for P in SDTJ.First_Priority .. SDTJ.Last_Priority loop
               Priority_Table (P) := SDTJ.Dispatching_Policy;
            end loop;
         end;
      end loop;

      --  For each continuous section of the Priority_Table that uses the same
      --  dispatching policy, record a new scheduler agent in the Scheduler
      --  Agent table.
      First_Priority := Priority_Table'First;
      Policy         := Priority_Table (First_Priority);
      for P in Priority_Table'Range loop
         if Policy /= Priority_Table (P) then
            SDR := (Dispatching_Policy => Policy,
                    First_Priority     => First_Priority,
                    Last_Priority      => P - 1,
                    PSD_Pragma_Line    => 0);
            Scheduler_Agents.Append (SDR);
            First_Priority := P;
            Policy := Priority_Table (P);
         end if;
      end loop;

      --  Add the last scheduler agent

      SDR := (Dispatching_Policy => Policy,
              First_Priority     => First_Priority,
              Last_Priority      => Priority_Table'Last,
              PSD_Pragma_Line    => 0);
      Scheduler_Agents.Append (SDR);
   end Set_Scheduler_Agent_Table;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String (S : String) is
   begin
      Statement_Buffer (Last + 1 .. Last + S'Length) := S;
      Last := Last + S'Length;
   end Set_String;

   ------------------------
   -- Set_String_Replace --
   ------------------------

   procedure Set_String_Replace (S : String) is
   begin
      Statement_Buffer (Last - S'Length + 1 .. Last) := S;
   end Set_String_Replace;

   -------------------
   -- Set_Unit_Name --
   -------------------

   procedure Set_Unit_Name is
   begin
      for J in 1 .. Name_Len - 2 loop
         if Name_Buffer (J) /= '.' then
            Set_Char (Name_Buffer (J));
         else
            Set_String ("__");
         end if;
      end loop;
   end Set_Unit_Name;

   ---------------------
   -- Set_Unit_Number --
   ---------------------

   procedure Set_Unit_Number (U : Unit_Id) is
      Num_Units : constant Nat := Nat (Units.Last) - Nat (Unit_Id'First);
      Unum      : constant Nat := Nat (U) - Nat (Unit_Id'First);

   begin
      if Num_Units >= 10 and then Unum < 10 then
         Set_Char ('0');
      end if;

      if Num_Units >= 100 and then Unum < 100 then
         Set_Char ('0');
      end if;

      Set_Int (Unum);
   end Set_Unit_Number;

   ----------------------
   -- Write_Info_Ada_C --
   ----------------------

   procedure Write_Info_Ada_C (Ada : String; C : String; Common : String) is
   begin
      if Ada_Bind_File then
         declare
            S : String (1 .. Ada'Length + Common'Length);
         begin
            S (1 .. Ada'Length) := Ada;
            S (Ada'Length + 1 .. S'Length) := Common;
            WBI (S);
         end;

      else
         declare
            S : String (1 .. C'Length + Common'Length);
         begin
            S (1 .. C'Length) := C;
            S (C'Length + 1 .. S'Length) := Common;
            WBI (S);
         end;
      end if;
   end Write_Info_Ada_C;

   ----------------------------
   -- Write_Statement_Buffer --
   ----------------------------

   procedure Write_Statement_Buffer is
   begin
      WBI (Statement_Buffer (1 .. Last));
      Last := 0;
   end Write_Statement_Buffer;

   procedure Write_Statement_Buffer (S : String) is
   begin
      Set_String (S);
      Write_Statement_Buffer;
   end Write_Statement_Buffer;

end Bindgen;
