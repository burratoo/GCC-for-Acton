with Atree;    use Atree;
with Checks;   use Checks;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Ch3;  use Exp_Ch3;
with Exp_Ch6;  use Exp_Ch6;
with Exp_Ch9;  use Exp_Ch9;
with Exp_Ch11; use Exp_Ch11;
with Exp_Dbug; use Exp_Dbug;
with Exp_Disp; use Exp_Disp;
with Exp_Sel;  use Exp_Sel;
with Exp_Smem; use Exp_Smem;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Freeze;   use Freeze;
with Hostparm;
with Itypes;   use Itypes;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch9;  use Sem_Ch9;
with Sem_Ch11; use Sem_Ch11;
with Sem_Elab; use Sem_Elab;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_Atom is

   function Action_Index_Expression
     (Sloc  : Source_Ptr;
      Act   : Entity_Id;
      Atm   : Entity_Id) return Node_Id;
   --  Compute the index position for an entry call.

   function Build_Action_Count_Expression
     (Atomic_Type     : Node_Id;
      Component_List  : List_Id;
      Loc             : Source_Ptr) return Node_Id;
   --  Compute number of actions for an atomic object. A single array with that
   --  size is allocated for each atomic object of the type.

   function Build_Action_Specification
     (N          : Node_Id;
      Atomic_Typ : Entity_Id;
      Kind       : Action_Sub_Kind) return Node_Id;
   --  Build the specification for actions. This is called when expanding an
   --  atomic type.

   function Build_Corresponding_Record
     (N    : Node_Id;
      Ctyp : Node_Id;
      Loc  : Source_Ptr) return Node_Id;
   --  Common to tasks and protected types. Copy discriminant specifications,
   --  build record declaration. N is the type declaration, Ctyp is the
   --  concurrent entity (task type or protected type).
   --  NOTE : This function is the same Exp_Ch9.

   procedure Build_External_Action_Body
     (N     : Node_Id;
      Pid   : Entity_Id;
      Ibody : Node_Id;
      Ebody : out Node_Id;
      Fstms : out List_Id);
   --  This procedure is used to construct the external version of an action.
   --  Its statement sequence first enters the associated atomic object, and
   --  then enters a block that contains a call to the internal version of the
   --  action (for details, see Build_Internal_Action_Body). This block
   --  statement requires a cleanup handler that exits the atomic action in all
   --  cases. Fstms provides the statements for this cleanup handler.
   --  (see Exp_Ch7.Expand_Cleanup_Actions).

   function Build_Internal_Action_Body
     (N   : Node_Id;
      Pid : Node_Id) return Node_Id;
   --  This routine constructs the internal version of an action body, which is
   --  contains all of the code in the original, unexpanded body. Called only
   --  by the external version of the action body.

   function Build_Selected_Name
     (Prefix      : Entity_Id;
      Selector    : Entity_Id;
      Append_Char : Character := ' ') return Name_Id;
   --  Build a name in the form of Prefix__Selector, with an optional
   --  character appended. This is used for internal subprograms generated
   --  for operations of atomic and protected types, including barrier
   --  functions. For the subprograms generated for entry bodies and entry
   --  barriers, the generated name includes a sequence number that makes names
   --  unique in the presence of entry overloading. This is necessary
   --  because entry body procedures and barrier functions all have the
   --  same signature.
   --  NOTE : This version will replace the one from Exp_Ch9.

   procedure Debug_Private_Data_Declarations (Decls : List_Id);
   --  Decls is a list which may contain the declarations created by Install_
   --  Private_Data_Declarations. All generated entities are marked as needing
   --  debug info and debug nodes are manually generation where necessary. This
   --  step of the expansion must to be done after private data has been moved
   --  to its final resting scope to ensure proper visibility of debug objects.
   --  NOTE : Copied from Exp_Ch9.

   function Find_Atomic_Declaration (Atyp : Entity_Id) return Node_Id;
   --  Returns the atomic declaration for the given atomic entitiy.

   function First_Action (D : List_Id) return Node_Id;
   --  Given the declarations list for an atomic body, find the
   --  first action body.

   -----------------------------
   -- Action_Index_Expression --
   -----------------------------

   function Action_Index_Expression
     (Sloc  : Source_Ptr;
      Act   : Entity_Id;
      Atm   : Entity_Id) return Node_Id
   is
      Atyp  : constant Entity_Id := Etype (Atm);
      Expr  : Node_Id;
      Count : Nat;
      Lo    : Node_Id;
      Hi    : Node_Id;
      Prev  : Entity_Id;
      S     : Node_Id;

   begin
      --  The queues and barriers of actions appear in textual order in the
      --  associated record. The action index is computed as the sum of the
      --  number of queues for all actions that precede the designated one.

      Prev  := First_Entity (Atyp);
      Count := 0;

      while Chars (Prev) /= Chars (Act)
        or else (Ekind (Prev) /= Ekind (Act))
        or else not Sem_Ch6.Type_Conformant (Act, Prev)
      loop
         if Ekind (Prev) = E_Action then
            Count := Count + 1;
         end if;

         Next_Entity (Prev);
      end loop;

      return Make_Integer_Literal (Sloc, Count);
   end Action_Index_Expression;

   -----------------------------------
   -- Build_Action_Count_Expression --
   -----------------------------------

   function Build_Action_Count_Expression
     (Atomic_Type     : Node_Id;
      Component_List  : List_Id;
      Loc             : Source_Ptr) return Node_Id
   is
      Aindx  : Nat;
      Ent    : Entity_Id;

   begin

      Aindx := 0;
      Ent := First_Entity (Atomic_Type);
      while Present (Ent) loop
         if Ekind (Ent) = E_Action then
            Aindx := Aindx + 1;
         end if;

         Next_Entity (Ent);
      end loop;

      return Make_Integer_Literal (Loc, Aindx);
   end Build_Action_Count_Expression;

   --------------------------------
   -- Build_Action_Specification --
   --------------------------------

   function Build_Action_Specification
     (N          : Node_Id;
      Atomic_Typ : Entity_Id;
      Kind       : Action_Sub_Kind) return Node_Id
   is
      Loc         : constant Source_Ptr := Sloc (N);
      Def_Id      : constant Entity_Id  := Defining_Identifier (N);
      Decl        : Node_Id;
      New_Id      : Entity_Id;
      New_Plist   : List_Id;
      New_Spec    : Node_Id;

      Append_Chr : constant array (Action_Sub_Kind) of Character :=
                     (Dispatching_Kind => ' ',
                      External_Kind    => 'E',
                      Internal_Kind    => 'I');

   begin
      if Nkind (N) = N_Action_Body then
         Decl := Unit_Declaration_Node
           (Corresponding_Spec (Action_Body_Formal_Part (N)));
      else
         Decl := N;
      end if;

      New_Plist := New_List;

      --  Build the spec.

      declare
         Formal    : Entity_Id;
         New_Decl  : Node_Id;
         New_Param : Node_Id;
      begin
         Formal := First_Formal (Def_Id);
         while Present (Formal) loop
            New_Param :=
              Make_Parameter_Specification (Loc,
                Defining_Identifier =>
                  Make_Defining_Identifier (Sloc (Formal), Chars (Formal)),
                In_Present          => In_Present (Parent (Formal)),
                Out_Present         => Out_Present (Parent (Formal)),
                Parameter_Type      => New_Reference_To (Etype (Formal), Loc));

            if Kind = Internal_Kind then
               Set_Atomic_Formal (Formal, Defining_Identifier (New_Param));
            end if;

            Append (New_Param, New_Plist);
            Next_Formal (Formal);
         end loop;

         New_Decl :=
           Make_Parameter_Specification (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uObject),
             In_Present  => True,
             Out_Present => True,
             Parameter_Type =>
               New_Reference_To (Corresponding_Record_Type (Atomic_Typ), Loc));
         Set_Debug_Info_Needed (Defining_Identifier (New_Decl));
         Prepend_To (New_Plist, New_Decl);
      end;

      New_Id :=
        Make_Defining_Identifier (Loc,
          Chars => Build_Selected_Name
            (Atomic_Typ, Def_Id, Append_Chr (Kind)));

      --  The internal operation carries the user code, and debugging
      --  information must be generated for it, even though this spec does
      --  not come from source. It is also convenient to allow gdb to step
      --  into the external operation, even though it only contains lock/
      --  unlock calls.

      Set_Debug_Info_Needed (New_Id);

      --  If a pragma Eliminate applies to the source entity, the internal
      --  subprograms will be eliminated as well.

      Set_Is_Eliminated (New_Id, Is_Eliminated (Def_Id));

      New_Spec :=
        Make_Procedure_Specification (Loc,
          Defining_Unit_Name => New_Id,
          Parameter_Specifications => New_Plist);

      return New_Spec;
   end Build_Action_Specification;

   --------------------------------
   -- Build_Corresponding_Record --
   --------------------------------

   function Build_Corresponding_Record
    (N    : Node_Id;
     Ctyp : Entity_Id;
     Loc  : Source_Ptr) return Node_Id
   is
      Rec_Ent  : constant Entity_Id :=
                   Make_Defining_Identifier
                     (Loc, New_External_Name (Chars (Ctyp), 'V'));
      Disc     : Entity_Id;
      Dlist    : List_Id;
      New_Disc : Entity_Id;
      Cdecls   : List_Id;

   begin
      Set_Corresponding_Record_Type     (Ctyp, Rec_Ent);
      Set_Ekind                         (Rec_Ent, E_Record_Type);
      Set_Has_Delayed_Freeze            (Rec_Ent, Has_Delayed_Freeze (Ctyp));
      Set_Is_Concurrent_Record_Type     (Rec_Ent, True);
      Set_Corresponding_Concurrent_Type (Rec_Ent, Ctyp);
      Set_Stored_Constraint             (Rec_Ent, No_Elist);
      Cdecls := New_List;

      --  Use discriminals to create list of discriminants for record, and
      --  create new discriminals for use in default expressions, etc. It is
      --  worth noting that a task discriminant gives rise to 5 entities;

      --  a) The original discriminant.
      --  b) The discriminal for use in the task.
      --  c) The discriminant of the corresponding record.
      --  d) The discriminal for the init proc of the corresponding record.
      --  e) The local variable that renames the discriminant in the procedure
      --     for the task body.

      --  In fact the discriminals b) are used in the renaming declarations
      --  for e). See details in einfo (Handling of Discriminants).

      if Present (Discriminant_Specifications (N)) then
         Dlist := New_List;
         Disc := First_Discriminant (Ctyp);

         while Present (Disc) loop
            New_Disc := CR_Discriminant (Disc);

            Append_To (Dlist,
              Make_Discriminant_Specification (Loc,
                Defining_Identifier => New_Disc,
                Discriminant_Type =>
                  New_Occurrence_Of (Etype (Disc), Loc),
                Expression =>
                  New_Copy (Discriminant_Default_Value (Disc))));

            Next_Discriminant (Disc);
         end loop;

      else
         Dlist := No_List;
      end if;

      --  Now we can construct the record type declaration. Note that this
      --  record is "limited tagged". It is "limited" to reflect the underlying
      --  limitedness of the task or protected object that it represents, and
      --  ensuring for example that it is properly passed by reference. It is
      --  "tagged" to give support to dispatching calls through interfaces. We
      --  propagate here the list of interfaces covered by the concurrent type
      --  (Ada 2005: AI-345).

      return
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Rec_Ent,
          Discriminant_Specifications => Dlist,
          Type_Definition =>
            Make_Record_Definition (Loc,
              Component_List =>
                Make_Component_List (Loc,
                  Component_Items => Cdecls),
              Limited_Present => True));
   end Build_Corresponding_Record;

   --------------------------------
   -- Build_External_Action_Body --
   --------------------------------

   procedure Build_External_Action_Body
     (N     : Node_Id;
      Pid   : Entity_Id;
      Ibody : Node_Id;
      Ebody : out Node_Id;
      Fstms : out List_Id)
   is
      Loc           : constant Source_Ptr := Sloc (N);
      Adec          : constant Node_Id    := Find_Atomic_Declaration (Pid);
      E             : constant Entity_Id  := Make_Temporary (Loc, 'E');
      E_True_Stmt   : Node_Id;
      E_Op_Spec     : Node_Id;
      Iactuals      : List_Id;
      Pformal       : Node_Id;
      Internal_Call : Node_Id;
      Sub_Type      : Node_Id;
      Action_Stmt   : Node_Id;
      Stmts         : List_Id;
      Object_Parm   : Node_Id;
      Exc_Safe      : Boolean;
      Decls         : List_Id := Empty_List;

      function Build_Atomic_Exception_Block return Node_Id;
      --  Builds the block that handles the detection and response of an atomic
      --  error.

      function New_Atomic_Error (Loc : Source_Ptr) return Node_Id;
      --  This function builds a tree corresponding to the Ada statement
      --  "raise Atomic_Error" and returns the root of this tree,
      --  the N_Raise_Statement node.

      ----------------------------------
      -- Build_Atomic_Exception_Block --
      ----------------------------------

      function Build_Atomic_Exception_Block return Node_Id is
         If_Stmt   : Node_Id;
         Handlers  : List_Id;
         Handler   : Node_Id;
         Block_Hs  : List_Id;
         E_Choice  : Node_Id;
         E_Choices : List_Id;
         Atom_Name : constant Name_Id :=
                       Chars (Standard_Entity (S_Atomic_Error));

      begin

         --  Build exception check.

         If_Stmt :=
           Make_Implicit_If_Statement (N,
             Condition       => New_Reference_To (E, Loc),
             Then_Statements => New_List (New_Atomic_Error (Loc)));

         --  Search for and extract Atomic_Error Handler from internal
         --  subprogram.

         Handlers := Exception_Handlers (Handled_Statement_Sequence (Ibody));
         Handler := First (Handlers);

         --  If no exception handlers are present in the internal subprogram's
         --  body then there is no Atomic_Error handler to copy.

         if not Present (Handler) then
            Block_Hs := New_List;

            --  Otherwise search for the Atomic_Error handler.

         else
            Handler_Loop :
            while Present (Handler) loop
               E_Choices := Exception_Choices (Handler);
               E_Choice := First (E_Choices);
               while Present (E_Choice) loop
                  if Chars (E_Choice) = Atom_Name then
                     Block_Hs := New_List (
                       Make_Exception_Handler (Loc,
                         Choice_Parameter  =>
                           New_Copy (Choice_Parameter (Handler)),
                         Exception_Choices =>
                           New_List (New_Copy (E_Choice)),
                         Statements        =>
                           New_Copy_List (Statements (Handler))));

                     --  If the exception handler only handles Atomic_Error,
                     --  we remove the handler.

                     if List_Length (E_Choices) = 1 then
                        Remove (Handler);

                        --  Otherwise, the handler handles other exceptions,
                        --  and we only remove the Atomic_Error choice.
                     else
                        Remove (E_Choice);
                     end if;

                     exit Handler_Loop;
                  end if;
               end loop;
            end loop Handler_Loop;
         end if;

         --  Build atomic exception block

         return Make_Block_Statement (Loc,
           Handled_Statement_Sequence =>
             Make_Handled_Sequence_Of_Statements (Loc,
               Statements         => New_List (If_Stmt),
               Exception_Handlers => Block_Hs));
      end Build_Atomic_Exception_Block;

      ----------------------
      -- New_Atomic_Error --
      ----------------------

      function New_Atomic_Error (Loc : Source_Ptr) return Node_Id is
         Ident_Node : Node_Id;
         Raise_Node : Node_Id;

      begin
         Ident_Node := New_Node (N_Identifier, Loc);
         Set_Chars (Ident_Node, Chars (Standard_Entity (S_Atomic_Error)));
         Set_Entity (Ident_Node, Standard_Entity (S_Atomic_Error));
         Raise_Node := New_Node (N_Raise_Statement, Loc);
         Set_Name (Raise_Node, Ident_Node);
         return Raise_Node;
      end New_Atomic_Error;

   begin
      E_Op_Spec :=
        Build_Action_Specification (N, Pid, External_Kind);

      --  Create declaration for exception flag. Also build the statement
      --  that sets the exception flag to True.

      Append_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier => E,
          Object_Definition   => New_Reference_To (Standard_Boolean, Loc),
          Expression          => New_Reference_To (Standard_False, Loc)));

      E_True_Stmt :=
        Make_Assignment_Statement (Loc,
          Name       => New_Reference_To (E, Loc),
          Expression => New_Reference_To (Standard_True, Loc));

      --  Create declaration for Action_Id.

      Append_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uAction_Id),
          Object_Definition   =>
            New_Reference_To (RTE (RE_Action_Index), Loc),
          Expression          =>
            Action_Index_Expression (Loc, Defining_Identifier (N), Pid)));

      --  Build a list of the formal parameters of the external version of
      --  the action to use as the actual parameters of the internal
      --  version.

      Iactuals := New_List;
      Pformal := First (Parameter_Specifications (E_Op_Spec));
      while Present (Pformal) loop
         Append_To (Iactuals,
           Make_Identifier (Loc, Chars (Defining_Identifier (Pformal))));
         Next (Pformal);
      end loop;

      --  Make a call to the unprotected version of the subprogram built above
      --  for use by the protected version built below.

      Internal_Call :=
        Make_Procedure_Call_Statement (Loc,
          Name =>
            Make_Identifier (Loc, Chars (Defining_Unit_Name (
              Specification (Ibody)))),
          Parameter_Associations => Iactuals);

      --  Wrap call in block that will be covered by an at_end handler. at_end
      --  handler created in Exp_Ch7.

      Internal_Call := Make_Block_Statement (Loc,
        Handled_Statement_Sequence =>
          Make_Handled_Sequence_Of_Statements (Loc,
            Statements => New_List (Internal_Call, E_True_Stmt)));

      --  Build call to Enter_Action.

      Object_Parm :=
        Make_Attribute_Reference (Loc,
          Attribute_Name => Name_Unchecked_Access,
          Prefix         =>
            Make_Selected_Component (Loc,
              Prefix        => Make_Identifier (Loc, Name_uObject),
              Selector_Name => Make_Identifier (Loc, Name_uObject)));

      Action_Stmt :=
        Make_Procedure_Call_Statement (Loc,
          Name                   =>
            New_Reference_To (RTE (RE_Enter_Action), Loc),
          Parameter_Associations =>  New_List (Object_Parm,
            Make_Identifier (Loc, Name_uAction_Id)));

      Stmts := New_List (Action_Stmt);

      Append (Internal_Call, Stmts);

      Ebody :=
        Make_Subprogram_Body (Loc,
          Declarations => Decls,
          Specification => E_Op_Spec,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc, Statements => Stmts));

      Set_Is_External_Action_Body (Ebody);

      --  Build finalisation statements.

      Fstms := New_List;

      if Has_End_Barrier (Adec) then
         Append_To (Fstms,
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Reference_To (RTE (RE_Action_End_Barrier), Loc),
             Parameter_Associations =>  New_List (New_Copy (Object_Parm),
               Make_Identifier (Loc, Name_uAction_Id),
               New_Reference_To (E, Loc))));
      end if;

      Append_To (Fstms, Build_Atomic_Exception_Block);

      Append_To (Fstms,
        Make_Procedure_Call_Statement (Loc,
          Name                   =>
            New_Reference_To (RTE (RE_Exit_Action), Loc),
          Parameter_Associations =>  New_List (New_Copy (Object_Parm),
            Make_Identifier (Loc, Name_uAction_Id))));

   end Build_External_Action_Body;

   --------------------------------
   -- Build_Internal_Action_Body --
   --------------------------------

   function Build_Internal_Action_Body
     (N   : Node_Id;
      Pid : Node_Id) return Node_Id
   is
      Decls : constant List_Id := Declarations (N);

   begin
      --  Add renamings for the Atomic object, discriminals, privals and
      --  for use by debugger.

      Debug_Private_Data_Declarations (Decls);

      --  Make an unprotected version of the subprogram for use within the same
      --  object, with a new name and an additional parameter representing the
      --  object.

      return
        Make_Subprogram_Body (Sloc (N),
          Specification              =>
            Build_Action_Specification (N, Pid, Internal_Kind),
          Declarations               => Decls,
          Handled_Statement_Sequence => Handled_Statement_Sequence (N));
   end Build_Internal_Action_Body;

   -------------------------
   -- Build_Selected_Name --
   -------------------------

   function Build_Selected_Name
     (Prefix      : Entity_Id;
      Selector    : Entity_Id;
      Append_Char : Character := ' ') return Name_Id
   is
      Select_Buffer : String (1 .. Hostparm.Max_Name_Length);
      Select_Len    : Natural;

   begin
      Get_Name_String (Chars (Selector));
      Select_Len := Name_Len;
      Select_Buffer (1 .. Select_Len) := Name_Buffer (1 .. Name_Len);
      Get_Name_String (Chars (Prefix));

      --  If scope is anonymous type, discard suffix to recover name of
      --  single atomic or protected object. Otherwise use atomic or protected
      --  type name.

      if Name_Buffer (Name_Len) = 'T' then
         Name_Len := Name_Len - 1;
      end if;

      Add_Str_To_Name_Buffer ("__");
      for J in 1 .. Select_Len loop
         Add_Char_To_Name_Buffer (Select_Buffer (J));
      end loop;

      --  Now add the Append_Char if specified. The encoding to follow
      --  depends on the type of entity.

      --  It would be better to encapsulate this as a routine in Exp_Dbug ???

      if Append_Char /= ' ' then
            Add_Char_To_Name_Buffer (Append_Char);
      end if;

      return Name_Find;
   end Build_Selected_Name;

   -------------------------------------
   -- Debug_Private_Data_Declarations --
   -------------------------------------

   procedure Debug_Private_Data_Declarations (Decls : List_Id) is
      Debug_Nod : Node_Id;
      Decl      : Node_Id;

   begin
      Decl := First (Decls);
      while Present (Decl)
        and then not Comes_From_Source (Decl)
      loop
         --  Declaration for the Protection object, discriminals, privals:
         --    conc_typR   : atomic_typ renames _object._object;
         --    discr_nameD : discr_typ renames _object.discr_name;
         --    discr_nameD : discr_typ renames _task.discr_name;
         --    prival_name : comp_typ  renames _object.comp_name;

         if Nkind (Decl) = N_Object_Renaming_Declaration then
            Set_Debug_Info_Needed (Defining_Identifier (Decl));
            Debug_Nod := Debug_Renaming_Declaration (Decl);

            if Present (Debug_Nod) then
               Insert_After (Decl, Debug_Nod);
            end if;
         end if;

         Next (Decl);
      end loop;
   end Debug_Private_Data_Declarations;

   -------------------------------------
   -- Expand_Atomic_Body_Declarations --
   -------------------------------------

   procedure Expand_Atomic_Body_Declarations
     (N       : Node_Id;
      Spec_Id : Entity_Id)
   is
   begin
      if No_Run_Time_Mode then
         Error_Msg_CRT ("atomic body", N);
         return;

      elsif Full_Expander_Active then

         --  Associate discriminals with the first action body to be expanded.

         if Present (First_Action (Declarations (N))) then
            Set_Discriminals (Parent (Spec_Id));
         end if;
      end if;
   end Expand_Atomic_Body_Declarations;

   --------------------------
   -- Expand_N_Action_Body --
   --------------------------

   procedure Expand_N_Action_Body (N : Node_Id) is
   begin
      --  Associate discriminals with the next action body to be
      --  expanded.

      if Present (Next_Action (N)) then
         Set_Discriminals (Parent (Current_Scope));
      end if;
   end Expand_N_Action_Body;

   ------------------------------------
   -- Expand_N_Action_Call_Statement --
   ------------------------------------

   procedure Expand_N_Action_Call_Statement (N : Node_Id) is
      Nam     : constant Node_Id := Name (N);
      Atomval : constant Node_Id := Prefix (Nam);
      Aname   : constant Node_Id := Selector_Name (Nam);
   begin
      Expand_Call (N);

      --  If call has been inlined, nothing left to do

      if Nkind (N) = N_Block_Statement then
         return;
      end if;

      --  Convert action call to a call to the action's external procedure

      declare
         Loc       : constant Source_Ptr := Sloc (N);
         Atomtyp   : Node_Id;
         Ent       : Entity_Id;
         New_Sub   : Node_Id;
         Params    : List_Id;
      begin

         Ent     := Entity (Aname);
         Atomtyp := Etype (Atomval);

         --  If prefix is an access type, dereference to obtain the atomic type

         if Is_Access_Type (Atomtyp) then
            Atomtyp := Designated_Type (Atomtyp);
         end if;

         --  Find the external subprogram for the action.

         declare
            Subp : constant Entity_Id := Action_Body_Subprogram (Ent);
         begin
            if Is_Access_Type (Next_Entity (Subp)) then
               New_Sub := Next_Entity (Next_Entity (Subp));
            else
               New_Sub := Next_Entity (Subp);
            end if;
         end;

         if Present (Parameter_Associations (N)) then
            Params := New_Copy_List_Tree (Parameter_Associations (N));
         else
            Params := New_List;
         end if;

         --  If the type is an untagged derived type, convert to the root type,
         --  which is the one on which the operations are defined.

         if Nkind (Atomval) = N_Unchecked_Type_Conversion
           and then not Is_Tagged_Type (Etype (Atomval))
           and then Is_Derived_Type (Etype (Atomval))
         then
            Set_Etype (Atomval, Root_Type (Etype (Atomval)));
            Set_Subtype_Mark (Atomval,
              New_Occurrence_Of (Root_Type (Etype (Atomval)), Sloc (N)));
         end if;

         Prepend (Atomval, Params);

         Rewrite (N,
           Make_Procedure_Call_Statement (Loc,
             Name                   => New_Sub,
             Parameter_Associations => Params));

         Analyze (N);
      end;
   end Expand_N_Action_Call_Statement;

   ----------------------------------------
   -- Expand_N_Alternative_Action_Select --
   ----------------------------------------

   --  A alternative action select:

   --    select action
   --       A.A;
   --       S1;
   --    else
   --       B.A;
   --       S2;
   --    else
   --       S3
   --    end select;

   --  is expanded as follow:

   --    declare
   --       F : Boolean := True;

   --    begin
   --       declare
   --       begin
   --          A.AE;
   --          F := False;
   --          S1;

   --       exception
   --          when Atomic_Error =>
   --             null;
   --       end;

   --       if not F then
   --          goto Exit;
   --       end if;

   --       declare
   --       begin
   --          B.AE;
   --          F := False;
   --          S2;

   --       exception
   --          when Atomic_Error =>
   --             null;
   --       end;

   --       if not F then
   --          goto Exit;
   --       end if;

   --       declare
   --       begin
   --          S3;
   --       end;

   --       Exit :
   --    end;

   procedure Expand_N_Alternative_Action_Select (N : Node_Id) is
      Loc            : constant Source_Ptr := Sloc (N);
      Alts           : constant List_Id := Action_Call_Alternatives (N);
      Els            : constant List_Id := Else_Statements (N);

      Alt            : Node_Id;

      Block          : Node_Id;
      Decls          : constant List_Id := New_List;
      Stmts          : constant List_Id := New_List;

      Exit_Lab       : Node_Id;
      F              : constant Node_Id := Make_Temporary (Loc, 'F');

      Act_Call_Stmts : List_Id := New_List;

      function Make_Flag_Check_Statement return Node_Id;
      --  Generate flag check statement:
      --    if not F then
      --       goto Exit;
      --    end if;

      function Make_Flag_Check_Statement return Node_Id is
      begin
         return Make_Implicit_If_Statement (N,
           Condition       => Make_Op_Not (Loc,
             Right_Opnd => New_Reference_To (F, Loc)),
           Then_Statements => New_List (
             Make_Goto_Statement (Loc,
               Name =>
                 New_Occurrence_Of (Entity (Identifier (Exit_Lab)), Loc))));
      end Make_Flag_Check_Statement;

   begin

      --  Generate label for common exit

      declare
         Exit_Id : Node_Id;

      begin
         Exit_Id := Make_Identifier (Loc, New_External_Name ('E', 0));
         Exit_Lab := Make_Label (Loc, Exit_Id);

         Append_To (Decls,
           Make_Implicit_Label_Declaration (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Chars (Exit_Id)),
             Label_Construct     => Exit_Lab));
      end;

      --  Loop through action call alternatives and transform them into
      --  the following block statement:
      --    declare
      --    begin
      --       Action_Call ();
      --       F := False;
      --       << statements >>
      --    exception
      --       when Atomic_Error =>
      --          null;
      --    end;

      Alt := First (Alts);
      while Present (Alt) loop
         --  Build block statments

         Append_To (Act_Call_Stmts,
           Relocate_Node (Action_Call_Statement (Alt)));
         Append_To (Act_Call_Stmts,
           Make_Assignment_Statement (Loc,
             Name       => New_Reference_To (F, Loc),
             Expression => New_Reference_To (Standard_True, Loc)));
         Append_List_To (Act_Call_Stmts,
           Copy_Separate_List (Statements (Alt)));

         Append_To (Stmts,
           Make_Block_Statement (Loc,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements         => Act_Call_Stmts,
                 Exception_Handlers => New_List (
                   Make_Implicit_Exception_Handler (Loc,
                     Exception_Choices => New_List (
                       New_Reference_To (Standard_Entity (S_Atomic_Error),
                         Loc)),
                     Statements        =>
                       New_List (Make_Null_Statement (Loc)))))));

         --  Add Flag check statement

         Append_To (Stmts, Make_Flag_Check_Statement);
      end loop;

      --  If we have else statements, append them now

      if Present (Els) then
         Append_To (Stmts,
           Make_Block_Statement (Loc,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Copy_Separate_List (Els))));
      end if;

      Append (Exit_Lab, Stmts);

      --  Replace accept statement with appropriate block

      Block :=
        Make_Block_Statement (Loc,
          Declarations => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stmts));

      Rewrite (N, Block);
      Analyze (N);
   end Expand_N_Alternative_Action_Select;

   --------------------------
   -- Expand_N_Atomic_Body --
   --------------------------

   --  Atomic bodies are expanded to the completion of the subprograms created
   --  for the corresponding atomic type. These are an external and internal
   --  version of each action in the object. For example, for atomic type
   --  atype :

   --  procedure actionI (_object : in out aoV;...) is
   --     <discriminant renamings>
   --     <private object renamings>
   --  begin
   --     <sequence of statements>
   --  end actionI;

   --  procedure actionE (_object : in out aoV;...) is
   --     Except : Boolean := True;
   --
   --     procedure _finalizer is
   --     begin
   --       Action_End_Barrier (_object._object, 1, Except);
   --       declare
   --       begin
   --          if Except then
   --             raise Atomic_Error;
   --          end if
   --       exception
   --          when Atomic_Error =>
   --             <statments>
   --       end;
   --       Exit_Action (_object._object, 1);
   --     end _finalizer;

   --  begin
   --     Enter_Action (_object._object, 1);
   --     actionI (_object;...);
   --     Execpt := False;
   --  at end
   --     _finalizer;
   --  end pproc;

   --  The type aoV is the record created for the atomic type to hold the state
   --  of the atomic object.

   procedure Expand_N_Atomic_Body (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Pid : constant Entity_Id  := Corresponding_Spec (N);

      Current_Node : Node_Id;
      Disp_Op_Body : Node_Id;
      New_Op_Body  : Node_Id;
      Num_Actions  : Natural := 0;
      Op_Body      : Node_Id;
      Op_Id        : Entity_Id;

      function Build_Dispatching_Action_Body
        (N           : Node_Id;
         Pid         : Node_Id;
         Action_Body : Node_Id) return Node_Id;
      --  Build a dispatching version of the action body. The newly generated
      --  subprogram contains a call to the external subprogram version of th
      --  action. The following code is generated:
      --
      --  procedure <action-name> (Param1 .. ParamN) is
      --  begin
      --     <action-procedure-name>E (Param1 .. ParamN);
      --  end <action-name>

      -----------------------------------
      -- Build_Dispatching_Action_Body --
      -----------------------------------

      function Build_Dispatching_Action_Body
        (N           : Node_Id;
         Pid         : Node_Id;
         Action_Body : Node_Id) return Node_Id
      is
         Loc     : constant Source_Ptr := Sloc (N);
         Actuals : List_Id;
         Formal  : Node_Id;
         Spec    : Node_Id;
         Stmts   : List_Id;

      begin
         --  Generate a specification without a letter suffix in order to
         --  override an interface function or procedure.

         Spec := Build_Action_Specification (N, Pid, Dispatching_Kind);

         --  The formal parameters become the actuals of the action call.

         Actuals := New_List;
         Formal  := First (Parameter_Specifications (Spec));
         while Present (Formal) loop
            Append_To (Actuals,
              Make_Identifier (Loc, Chars (Defining_Identifier (Formal))));
            Next (Formal);
         end loop;

         Stmts :=
           New_List (
             Make_Procedure_Call_Statement (Loc,
               Name =>
                 New_Reference_To (Corresponding_Spec (Action_Body), Loc),
               Parameter_Associations => Actuals));

         return
           Make_Subprogram_Body (Loc,
             Declarations               => Empty_List,
             Specification              => Spec,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc, Stmts));
      end Build_Dispatching_Action_Body;

   --  Start of processing for Expand_N_Atomic_Body

   begin
      if No_Run_Time_Mode then
         Error_Msg_CRT ("atomic body", N);
         return;
      end if;

      --  This is the proper body corresponding to a stub. The declarations
      --  must be inserted at the point of the stub, which in turn is in the
      --  declarative part of the parent unit.

      if Nkind (Parent (N)) = N_Subunit then
         Current_Node := Corresponding_Stub (Parent (N));
      else
         Current_Node := N;
      end if;

      Op_Body := First (Declarations (N));

      --  The atomic body is replaced with the bodies of its atomic operations.

      Rewrite (N, Make_Null_Statement (Sloc (N)));
      Analyze (N);

      while Present (Op_Body) loop
         case Nkind (Op_Body) is
            when N_Subprogram_Declaration | N_Subprogram_Body =>
               null;

            when N_Action_Body =>
               declare
                  Ibody : Node_Id;
                  Fstms : List_Id;
               begin
                  Ibody :=
                    Build_Internal_Action_Body (Op_Body, Pid);

                  Insert_After (Current_Node, Ibody);
                  Current_Node := Ibody;

                  Build_External_Action_Body
                    (Op_Body, Pid, Ibody, New_Op_Body, Fstms);

                  Insert_After (Current_Node, New_Op_Body);
                  Analyze (Ibody);
                  Analyze (New_Op_Body);

                  Set_Finalization_Statements
                    (Corresponding_Spec (New_Op_Body), Fstms);
                  Current_Node := New_Op_Body;
               end;

               --  Generate an overriding primitive operation body for
               --  this subprogram if the atomic type implements an
               --  interface.

               if Ada_Version >= Ada_2005
                    and then
                  Present (Interfaces (Corresponding_Record_Type (Pid)))
               then
                  Disp_Op_Body :=
                    Build_Dispatching_Action_Body
                      (Op_Body, Pid, New_Op_Body);

                  Insert_After (Current_Node, Disp_Op_Body);
                  Analyze (Disp_Op_Body);

                  Current_Node := Disp_Op_Body;
               end if;
            when N_Implicit_Label_Declaration =>
               null;

            when N_Itype_Reference =>
               Insert_After (Current_Node, New_Copy (Op_Body));

            when N_Freeze_Entity =>
               New_Op_Body := New_Copy (Op_Body);

               if Present (Entity (Op_Body))
                 and then Freeze_Node (Entity (Op_Body)) = Op_Body
               then
                  Set_Freeze_Node (Entity (Op_Body), New_Op_Body);
               end if;

               Insert_After (Current_Node, New_Op_Body);
               Current_Node := New_Op_Body;
               Analyze (New_Op_Body);

            when N_Pragma =>
               New_Op_Body := New_Copy (Op_Body);
               Insert_After (Current_Node, New_Op_Body);
               Current_Node := New_Op_Body;
               Analyze (New_Op_Body);

            when N_Object_Declaration =>
               pragma Assert (not Comes_From_Source (Op_Body));
               New_Op_Body := New_Copy (Op_Body);
               Insert_After (Current_Node, New_Op_Body);
               Current_Node := New_Op_Body;
               Analyze (New_Op_Body);

            when others =>
               raise Program_Error;

         end case;

         Next (Op_Body);
      end loop;
   end Expand_N_Atomic_Body;

   --------------------------------------
   -- Expand_N_Atomic_Type_Declaration --
   --------------------------------------

   --  First we create a corresponding record type declaration used to
   --  represent values of this atomic type.
   --  The general form of this type declaration is

   --    type aoV (discriminants) is record
   --      _Object       : aliased <kind>Atomic_Object
   --         [(<action_count>)];
   --      <private data fields>
   --    end record;

   --  The discriminants are present only if the corresponding atomic type
   --  has discriminants, and they exactly mirror the atomic type
   --  discriminants. The private data fields similarly mirror the private
   --  declarations of the atomic type.

   --  The Object field is always present. It contains RTS specific data used
   --  to control the atomic object. It is declared as Aliased so that it
   --  can be passed as a pointer to the RTS. This allows the atomic record
   --  to be referenced within RTS data structures. An appropriate
   --  Atomic_Object type and discriminant are generated.

   --  When an atomic object is declared, an instance of the atomic type
   --  value record is created. The initialization routine for the atomic type
   --  itself then calls Initialize_Atomic with appropriate parameters to
   --  initialize the the object's flags.

   --  Note: this record is passed to the subprograms created by the expansion
   --  of actions. It is an in out parameter to action bodies. The Entity_Id
   --  for this created record type is placed in the Corresponding_Record_Type
   --  field of the associated atomic type entity.

   --  Next we create a procedure specifications for actions. For each action
   --  two subprograms are created, an internal version and an external
   --  version. The internal version is called by the external version only.

   --  procedure actionI (_object : in out poV);
   --  procedure actionE (_object : in out poV);

   --  Note that this must come after the record type declaration, since
   --  the specs refer to this type.

   procedure Expand_N_Atomic_Type_Declaration (N : Node_Id) is
      Loc        : constant Source_Ptr := Sloc (N);
      Atomic_Typ : constant Entity_Id  := Defining_Identifier (N);

      Adef : constant Node_Id := Atomic_Definition (N);
      --  This contains two lists; one for visible and one for private decls

      Rec_Decl     : Node_Id;
      Cdecls       : List_Id;
      Discr_Map    : constant Elist_Id := New_Elmt_List;
      Priv         : Node_Id;
      New_Priv     : Node_Id;
      Comp         : Node_Id;
      Comp_Id      : Entity_Id;
      Sub          : Node_Id;
      Current_Node : Node_Id := N;
      Bdef         : Entity_Id := Empty; -- avoid uninit warning
      Edef         : Entity_Id := Empty; -- avoid uninit warning
      Body_Id      : Entity_Id;
      Body_Arr     : Node_Id;
      A_Count      : Int;
      Object_Comp  : Node_Id;

      --  NOTE: When merging this package with Exp_Ch9, these subprograms
      --  should be pulled out and place at the package level as they are used
      --  by Expand_N_Atomic_Type_Declaration.

      function Static_Component_Size (Comp : Entity_Id) return Boolean;
      --  When compiling under the Ravenscar profile, private components must
      --  have a static size, or else a protected object  will require heap
      --  allocation, violating the corresponding restriction. It is preferable
      --  to make this check here, because it provides a better error message
      --  than the back-end, which refers to the object as a whole.

      procedure Move_PPC_List (From_Entity   : Entity_Id;
                               To_Subprogram : Node_Id);
      --  Move preconditions and postconditions from the action definition to
      --  the internal subprogram. This is carried out as the preconditions
      --  need to be checked before checking to see if the entry barrier is
      --  open (See RM 6.1.1, par 23.3).

      ---------------------------------
      -- Check_Static_Component_Size --
      ---------------------------------

      function Static_Component_Size (Comp : Entity_Id) return Boolean is
         Typ : constant Entity_Id := Etype (Comp);
         C   : Entity_Id;

      begin
         if Is_Scalar_Type (Typ) then
            return True;

         elsif Is_Array_Type (Typ) then
            return Compile_Time_Known_Bounds (Typ);

         elsif Is_Record_Type (Typ) then
            C := First_Component (Typ);
            while Present (C) loop
               if not Static_Component_Size (C) then
                  return False;
               end if;

               Next_Component (C);
            end loop;

            return True;

         --  Any other type will be checked by the back-end

         else
            return True;
         end if;
      end Static_Component_Size;

      -------------------
      -- Move_PPC_List --
      -------------------

      procedure Move_PPC_List (From_Entity   : Entity_Id;
                               To_Subprogram : Node_Id) is
         PPC : constant Node_Id := Spec_PPC_List (Contract (From_Entity));
         C   : constant Node_Id :=
                 Make_Contract (Sloc (From_Entity),
                                Spec_PPC_List => Relocate_Node (PPC));
      begin
         Set_Contract (Defining_Unit_Name (Specification (To_Subprogram)), C);
      end Move_PPC_List;

      --  Start of processing for Expand_N_Atomic_Type_Declaration

   begin
      if Present (Corresponding_Record_Type (Atomic_Typ)) then
         return;
      else
         Rec_Decl := Build_Corresponding_Record (N, Atomic_Typ, Loc);
      end if;

      Cdecls := Component_Items (Component_List (Type_Definition (Rec_Decl)));

      Qualify_Entity_Names (N);

      --  If the type has discriminants, their occurrences in the declaration
      --  have been replaced by the corresponding discriminals. For components
      --  that are constrained by discriminants, their homologues in the
      --  corresponding record type must refer to the discriminants of that
      --  record, so we must apply a new renaming to subtypes_indications:

      --     atomic discriminant => discriminal => record discriminant

      --  This replacement is not applied to default expressions, for which
      --  the discriminal is correct.

      if Has_Discriminants (Atomic_Typ) then
         declare
            Disc : Entity_Id;
            Decl : Node_Id;

         begin
            Disc := First_Discriminant (Atomic_Typ);
            Decl := First (Discriminant_Specifications (Rec_Decl));
            while Present (Disc) loop
               Append_Elmt (Discriminal (Disc), Discr_Map);
               Append_Elmt (Defining_Identifier (Decl), Discr_Map);
               Next_Discriminant (Disc);
               Next (Decl);
            end loop;
         end;
      end if;

      --  Fill in the component declarations

      pragma Assert (Present (Adef));

      --  Add private field components

      if Present (Private_Declarations (Adef)) then
         Priv := First (Private_Declarations (Adef));

         while Present (Priv) loop

            if Nkind (Priv) = N_Component_Declaration then
               if not Static_Component_Size (Defining_Identifier (Priv)) then

                  --  When compiling for a restricted profile, the private
                  --  components must have a static size. If not, this is an
                  --  error for a single atomic declaration, and rates a
                  --  warning on a atomic type declaration.

                  if not Comes_From_Source (Atomic_Typ) then
                     Check_Restriction (No_Implicit_Heap_Allocations, Priv);

                  elsif Restriction_Active (No_Implicit_Heap_Allocations) then
                     Error_Msg_N ("component has non-static size?", Priv);
                     Error_Msg_NE
                       ("\creation of atomic object of type& will violate"
                        & " restriction No_Implicit_Heap_Allocations?",
                        Priv, Atomic_Typ);
                  end if;
               end if;

               --  The component definition consists of a subtype indication,
               --  or (in Ada 2005) an access definition. Make a copy of the
               --  proper definition.

               declare
                  Old_Comp : constant Node_Id   := Component_Definition (Priv);
                  Oent     : constant Entity_Id := Defining_Identifier (Priv);
                  New_Comp : Node_Id;
                  Nent     : constant Entity_Id :=
                               Make_Defining_Identifier (Sloc (Oent),
                                 Chars => Chars (Oent));

               begin
                  if Present (Subtype_Indication (Old_Comp)) then
                     New_Comp :=
                       Make_Component_Definition (Sloc (Oent),
                         Aliased_Present    => False,
                         Subtype_Indication =>
                           New_Copy_Tree (Subtype_Indication (Old_Comp),
                                           Discr_Map));
                  else
                     New_Comp :=
                       Make_Component_Definition (Sloc (Oent),
                         Aliased_Present    => False,
                         Access_Definition  =>
                           New_Copy_Tree (Access_Definition (Old_Comp),
                                           Discr_Map));
                  end if;

                  New_Priv :=
                    Make_Component_Declaration (Loc,
                      Defining_Identifier  => Nent,
                      Component_Definition => New_Comp,
                      Expression           => Expression (Priv));

                  Set_Has_Per_Object_Constraint (Nent,
                    Has_Per_Object_Constraint (Oent));

                  Append_To (Cdecls, New_Priv);
               end;
            end if;

            Next (Priv);
         end loop;
      end if;

      --  Prepend the _Object field with the right type to the component list.
      --  We need to compute the number of actions.

      declare
         Atomic_Subtype    : Node_Id;
         Action_Count_Expr : constant Node_Id :=
                               Build_Action_Count_Expression
                                 (Atomic_Typ, Cdecls, Loc);

      begin
         Atomic_Subtype :=
           Make_Subtype_Indication (Loc,
             Subtype_Mark =>
               New_Reference_To (RTE (RE_Atomic_Object), Loc),
             Constraint   =>
               Make_Index_Or_Discriminant_Constraint (
                 Sloc        => Loc,
                 Constraints => New_List (Action_Count_Expr)));

         Object_Comp :=
           Make_Component_Declaration (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uObject),
             Component_Definition =>
               Make_Component_Definition (Loc,
                 Aliased_Present    => True,
                 Subtype_Indication => Atomic_Subtype));
      end;

      --  Put the _Object component after the private component so that it
      --  be finalized early as required by 9.x (xx)

      Append_To (Cdecls, Object_Comp);

      Insert_After (Current_Node, Rec_Decl);
      Current_Node := Rec_Decl;

      --  Analyze the record declaration immediately after construction,
      --  because the initialization procedure is needed for single object
      --  declarations before the next entity is analyzed (the freeze call
      --  that generates this initialization procedure is found below).

      Analyze (Rec_Decl, Suppress => All_Checks);

      --  Build two new procedure specifications for each action; one to call
      --  from outside the object and house the compiler generated calls to
      --  enter and exit the action, and one that is called by the former and
      --  the pre/post-conditions.

      A_Count := 0;
      Comp := First (Visible_Declarations (Adef));
      while Present (Comp) loop
         if Nkind (Comp) = N_Action_Declaration then
            Comp_Id := Defining_Identifier (Comp);

            Sub :=
              Make_Subprogram_Declaration (Loc,
                Specification =>
                  Build_Action_Specification
                    (Comp, Atomic_Typ, Internal_Kind));

            --  Pre/postconditons are attached to the internal subprogram.

            Move_PPC_List (From_Entity => Comp_Id, To_Subprogram => Sub);

            Insert_After (Current_Node, Sub);
            Analyze (Sub);

            Set_Action_Body_Subprogram
              (Comp_Id, Defining_Unit_Name (Specification (Sub)));

            --  Make the external version of the subprogram available for
            --  expansion of external calls.

            Current_Node := Sub;

            Sub :=
              Make_Subprogram_Declaration (Loc,
                Specification =>
                  Build_Action_Specification
                    (Comp, Atomic_Typ, External_Kind));
            Insert_After (Current_Node, Sub);
            Analyze (Sub);

            Current_Node := Sub;
         end if;

         Next (Comp);
      end loop;
   end Expand_N_Atomic_Type_Declaration;

   -----------------------------
   -- Find_Atomic_Declaration --
   -----------------------------

   function Find_Atomic_Declaration (Atyp : Entity_Id) return Node_Id is
      Adef : Node_Id;
      Adec : Node_Id;
   begin
      --  Get atomic declaration. In the case of an atomic type declaration,
      --  this is simply the parent of the atomic type entity. In the single
      --  atomic object declaration, this parent will be the implicit type,
      --  and we can find the corresponding single atomic object declaration
      --  by searching forward in the declaration list in the tree.

      --  Is the test for N_Single_Atomic_Declaration needed here??? Nodes
      --  of this type should have been removed during semantic analysis.

      Adec := Parent (Atyp);
      while not Nkind_In (Adec, N_Atomic_Type_Declaration,
                                N_Single_Atomic_Declaration)
      loop
         Next (Adec);
      end loop;

      --  Now we can return the object definition from this declaration

      return Atomic_Definition (Adec);
   end Find_Atomic_Declaration;

   ------------------
   -- First_Action --
   ------------------

   function First_Action (D : List_Id) return Node_Id is
      First_Op : Node_Id;

   begin
      First_Op := First (D);
      while Present (First_Op)
        and then Nkind (First_Op) /= N_Action_Body
      loop
         Next (First_Op);
      end loop;

      return First_Op;
   end First_Action;

   ----------------------------
   -- Make_Initialize_Atomic --
   ----------------------------

   function Make_Initialize_Atomic
     (Atomic_Rec : Entity_Id) return List_Id
   is
      Loc         : constant Source_Ptr := Sloc (Atomic_Rec);
      A_Arr       : Entity_Id;
      Adef        : Node_Id;
      Adec        : Node_Id;
      Atyp        : constant Node_Id :=
                      Corresponding_Concurrent_Type (Atomic_Rec);
      B           : Node_Id;
      Args        : List_Id;
      L           : constant List_Id := New_List;

   begin
      Adef := Find_Atomic_Declaration (Atyp);

      --  Build the parameter list for the call. Note that _Init is the name
      --  of the formal for the object to be initialized, which is the task
      --  value record itself.

      Args := New_List;

      --  Object parameter. This is a pointer to the object of type
      --  Atomic used by the Oak to control the atomic object.

      Append_To (Args,
        Make_Attribute_Reference (Loc,
          Prefix =>
            Make_Selected_Component (Loc,
              Prefix        => Make_Identifier (Loc, Name_uInit),
              Selector_Name => Make_Identifier (Loc, Name_uObject)),
          Attribute_Name => Name_Unchecked_Access));

      --  Parent parameter. Points to the objtect's parent atomic object.
      --  Currently not implemented.

      Append_To (Args, Make_Null (Loc));

      --  Start and end barrier parameters. Sets whether or not the atomic
      --  object has a start and/or end barrier.

      if Has_End_Barrier (Adef) then
         B := New_Occurrence_Of (Standard_True, Loc);
      else
         B := New_Occurrence_Of (Standard_False, Loc);
      end if;

      Append_To (Args, B);

      if Has_Start_Barrier (Adef) then
         B := New_Occurrence_Of (Standard_True, Loc);
      else
         B := New_Occurrence_Of (Standard_False, Loc);
      end if;

      Append_To (Args, B);

      --  Require all actions parameter.

      if Require_All_Actions (Adef) then
         B := New_Occurrence_Of (Standard_True, Loc);
      else
         B := New_Occurrence_Of (Standard_False, Loc);
      end if;

      Append_To (Args, B);

      Append_To (L,
        Make_Procedure_Call_Statement (Loc,
          Name                   =>
            New_Reference_To (RTE (RE_Initialise_Atomic_Object), Loc),
          Parameter_Associations => Args));

      return L;
   end Make_Initialize_Atomic;

   -----------------
   -- Next_Action --
   -----------------

   function Next_Action (N : Node_Id) return Node_Id is
      Next_Op : Node_Id;

   begin
      Next_Op := Next (N);
      while Present (Next_Op)
        and then Nkind (Next_Op) /= N_Action_Body
      loop
         Next (Next_Op);
      end loop;

      return Next_Op;
   end Next_Action;

end Exp_Atom;
