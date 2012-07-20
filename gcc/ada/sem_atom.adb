with Atree;    use Atree;
with Checks;   use Checks;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Atom; use Exp_Atom;
with Exp_Ch9;  use Exp_Ch9;
with Elists;   use Elists;
with Freeze;   use Freeze;
with Lib.Xref; use Lib.Xref;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch5;  use Sem_Ch5;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch13; use Sem_Ch13;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Snames;   use Snames;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Style;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Sem_Atom is

   procedure Check_Max_Actions (D : Node_Id; R : All_Parameter_Restrictions);
   --  Given an atomic definition in D, check the corresponding restriction
   --  parameter identifier R, and if it is set, count the entries
   --  (checking the static requirement), and compare with the given maximum.

   --  The following are copied from Sem_Ch9, with the intention to move
   --  the contents of this package over to Sem_Ch9. Duplicating them here
   --  means that we don't have to make these subprograms visable in Sem_Ch9,
   --  which would require us to reverse the changes when we merge this
   --  package into Sem_Ch9.

   function Find_Concurrent_Spec (Body_Id : Entity_Id) return Entity_Id;
   --  Find entity in corresponding atomic declaration. Use full view if first
   --  declaration was for an incomplete type.

   procedure Install_Declarations (Spec : Entity_Id);
   --  Utility to make visible in corresponding body the entities defined in
   --  atomic declaration.

   -------------------------
   -- Analyze_Action_Body --
   -------------------------

   procedure Analyze_Action_Body (N : Node_Id) is
      Id          : constant Entity_Id := Defining_Identifier (N);
      Decls       : constant List_Id   := Declarations (N);
      Stats       : constant Node_Id   := Handled_Statement_Sequence (N);
      Formals     : constant Node_Id   := Action_Body_Formal_Part (N);
      A_Type      : constant Entity_Id := Current_Scope;
      E           : Entity_Id;
      Action_Name : Entity_Id;

   begin
      Tasking_Used := True;

      --  Action_Name is initialized to Any_Id. It should get reset to the
      --  matching action entity. An error is signalled if it is not reset

      Action_Name := Any_Id;

      Analyze (Formals);

      Set_Ekind          (Id, E_Action);
      Set_Scope          (Id, Current_Scope);
      Set_Etype          (Id, Standard_Void_Type);

      E := First_Entity (A_Type);
      while Present (E) loop
         if Chars (E) = Chars (Id)
           and then (Ekind (E) = Ekind (Id))
           and then Type_Conformant (Id, E)
         then
            Action_Name := E;
            Set_Convention (Id, Convention (E));
            Set_Corresponding_Spec (Action_Body_Formal_Part (N), Action_Name);
            Set_Corresponding_Body (Parent (Action_Name), Id);
            Check_Fully_Conformant (Id, E, N);
            exit;
         end if;

         Next_Entity (E);
      end loop;

      if Action_Name = Any_Id then
         Error_Msg_N ("no action declaration matches action body",  N);
         return;

      elsif Has_Completion (Action_Name) then
         Error_Msg_N ("duplicate action body", N);
         return;

      else
         Set_Has_Completion (Action_Name);
         Generate_Reference (Action_Name, Id, 'b', Set_Ref => False);
         Style.Check_Identifier (Id, Action_Name);
      end if;

      Push_Scope (Action_Name);

      Install_Declarations (Action_Name);
      Set_Actual_Subtypes (N, Current_Scope);

      --  The entity for the subprogram corresponding to the action has been
      --  created. We retain the name of this entity in the action body, for
      --  use when the corresponding subprogram body is created. Note that
      --  action bodies have no corresponding_spec, and there is no easy link
      --  back in the tree between the action body and the entity for the
      --  action itself, which is why we must propagate some attributes
      --  explicitly from spec to body.

      Set_Action_Body_Subprogram
        (Id, Action_Body_Subprogram (Action_Name));

      --  Add a declaration for the Atomic object, renaming declarations
      --  for the discriminals and privals.

      if Full_Expander_Active
        and then Is_Atomic_Type (A_Type)
      then
         Install_Private_Data_Declarations
           (Sloc (N), Action_Name, A_Type, N, Decls);
      end if;

      if Present (Decls) then
         Analyze_Declarations (Decls);
         Inspect_Deferred_Constant_Completion (Decls);
      end if;

      if Present (Stats) then
         Expand_Action_Body_Statements (N);
         Analyze (Stats);
      end if;

      --  Check for unreferenced variables etc. Before the Check_References
      --  call, we transfer Never_Set_In_Source and Referenced flags from
      --  parameters in the spec to the corresponding entities in the body,
      --  since we want the warnings on the body entities. Note that we do
      --  not have to transfer Referenced_As_LHS, since that flag can only
      --  be set for simple variables.

      --  At the same time, we set the flags on the spec entities to suppress
      --  any warnings on the spec formals, since we also scan the spec.

      declare
         E1 : Entity_Id;
         E2 : Entity_Id;

      begin
         E1 := First_Entity (Action_Name);
         while Present (E1) loop
            E2 := First_Entity (Id);
            while Present (E2) loop
               exit when Chars (E1) = Chars (E2);
               Next_Entity (E2);
            end loop;

            --  If no matching body entity, then we already had a detected
            --  error of some kind, so just don't worry about these warnings.

            if No (E2) then
               goto Continue;
            end if;

            if Ekind (E1) = E_Out_Parameter then
               Set_Never_Set_In_Source (E2, Never_Set_In_Source (E1));
               Set_Never_Set_In_Source (E1, False);
            end if;

            Set_Referenced (E2, Referenced (E1));
            Set_Referenced (E1);

         <<Continue>>
            Next_Entity (E1);
         end loop;

         Check_References (Id);
      end;

      --  We still need to check references for the spec, since objects
      --  declared in the body are chained (in the First_Entity sense) to
      --  the spec rather than the body in the case of actions.

      Check_References (Action_Name);

      --  Process the end label, and terminate the scope

      Process_End_Label (Handled_Statement_Sequence (N), 't', Action_Name);
      End_Scope;

   end Analyze_Action_Body;

   -------------------------------------
   -- Analyze_Action_Body_Formal_Part --
   -------------------------------------

   procedure Analyze_Action_Body_Formal_Part (N : Node_Id) is
      Id      : constant Entity_Id := Defining_Identifier (Parent (N));
      Formals : constant List_Id   := Parameter_Specifications (N);

   begin
      Tasking_Used := True;

      if Present (Formals) then
         Set_Scope (Id, Current_Scope);
         Push_Scope (Id);
         Process_Formals (Formals, Parent (N));
         End_Scope;
      end if;
   end Analyze_Action_Body_Formal_Part;

   -------------------------------------
   -- Analyze_Action_Call_Alternative --
   -------------------------------------

   procedure Analyze_Action_Call_Alternative (N : Node_Id) is
      Call : constant Node_Id := Action_Call_Statement (N);
   begin
      Tasking_Used := True;

      Analyze (Call);

      if Comes_From_Source (Call)
        and then Nkind (Call) /= N_Action_Call_Statement
      then
         Error_Msg_N ("action call alternative requires an action call", Call);
         return;
      end if;

      if Is_Non_Empty_List (Statements (N)) then
         Analyze_Statements (Statements (N));
      end if;

   end Analyze_Action_Call_Alternative;

   --------------------------------
   -- Analyze_Action_Declaration --
   --------------------------------

   procedure Analyze_Action_Declaration (N : Node_Id) is
      Def_Id  : constant Entity_Id := Defining_Identifier (N);
      Formals : constant List_Id   := Parameter_Specifications (N);

   begin
      Generate_Definition (Def_Id);
      Set_Contract (Def_Id, Make_Contract (Sloc (Def_Id)));
      Tasking_Used := True;

      --  Decorate Def_Id

      Set_Ekind          (Def_Id, E_Action);
      Set_Etype          (Def_Id, Standard_Void_Type);
      Set_Convention     (Def_Id, Convention_Action);

      --  Process formals

      if Present (Formals) then
         Set_Scope (Def_Id, Current_Scope);
         Push_Scope (Def_Id);
         Process_Formals (Formals, N);
         Create_Extra_Formals (Def_Id);
         End_Scope;
      end if;

      New_Overloaded_Entity (Def_Id);

      Generate_Reference_To_Formals (Def_Id);

      if Has_Aspects (N) then
         Analyze_Aspect_Specifications (N, Def_Id);
      end if;

      --  An action inherts its atomic parent's Restore_State aspect if it
      --  does not specify its own. To achieve this we chain the atomic type's
      --  rep item chain to the end of the action's rep item chain. Since
      --  the rep item chain has not been populated yet, we can just place
      --  the atomic rep item chain to the front.

      Set_First_Rep_Item (Def_Id, First_Rep_Item (Scope (Def_Id)));

   end Analyze_Action_Declaration;

   -------------------------------------
   -- Analyze_Alternative_Action_Call --
   -------------------------------------

   procedure Analyze_Alternative_Action_Select (N : Node_Id) is
      Alt_List : constant List_Id := Action_Call_Alternatives (N);
      Alt      : Node_Id;
      Stm      : Node_Id;

   begin
      Tasking_Used := True;
      --  Check_SPARK_Restriction ("action call is not allowed", N);
      Check_Restriction (No_Select_Statements, N);

      if Present (Pragmas_Before (N)) then
         Analyze_List (Pragmas_Before (N));
      end if;

      --  Analyze alternatives

      Analyze_List (Alt_List);

      --  Analyze sequence of statement

      if Is_Non_Empty_List (Statements (N)) then
         Analyze_Statements (Statements (N));

         --  The parser cannot tell if the last else statement is an
         --  action_call_alternative or just a normal sequence of statements.
         --  We do the check here and transform from a Sequence_of_Statements
         --  to an Action_Call_Alternative.

         Stm := First (Statements (N));

         if Nkind (Stm) = N_Action_Call_Statement then
            Alt := Make_Action_Call_Alternative (Sloc (Stm),
                     Action_Call_Statement => Relocate_Node (Stm),
                     Statements            => Statements (N));
            Set_Analyzed (Alt, True);
            Append_To (Alt_List, Alt);
         end if;
      end if;
   end Analyze_Alternative_Action_Select;

   -------------------------
   -- Analyze_Atomic_Body --
   -------------------------

   procedure Analyze_Atomic_Body (N : Node_Id) is
      Body_Id : constant Entity_Id := Defining_Identifier (N);
      Aspect  : Node_Id;
      Last_E  : Entity_Id;

      Spec_Id : Entity_Id;
      --  This is initially the entity of the atomic object or atomic type
      --  involved, but is replaced by the protected type always in the case
      --  of a single atomic declaration, since this is the proper scope to be
      --  used.

      Ref_Id : Entity_Id;
      --  This is the entity of the atomic object or atomic type involved, and
      --  is the entity used for cross - reference purposes (it differs from
      --  Spec_Id in the case of a single atomic object, since Spec_Id is
      --  set to the atomic type in this case).

   begin
      Tasking_Used := True;
      Set_Ekind (Body_Id, E_Atomic_Body);
      Spec_Id := Find_Concurrent_Spec (Body_Id);

      if Present (Spec_Id)
        and then Ekind (Spec_Id) = E_Atomic_Type
      then
         null;

      elsif Present (Spec_Id)
        and then Ekind (Etype (Spec_Id)) = E_Atomic_Type
        and then not Comes_From_Source (Etype (Spec_Id))
      then
         null;

      else
         Error_Msg_N ("missing specification for atomic body", Body_Id);
         return;
      end if;

      Ref_Id := Spec_Id;
      Generate_Reference (Ref_Id, Body_Id, 'b', Set_Ref => False);
      Style.Check_Identifier (Body_Id, Spec_Id);

      --  The declarations are always attached to the type

      if Ekind (Spec_Id) /= E_Atomic_Type then
         Spec_Id := Etype (Spec_Id);
      end if;

      Push_Scope (Spec_Id);
      Set_Corresponding_Spec (N, Spec_Id);
      Set_Corresponding_Body (Parent (Spec_Id), Body_Id);
      Set_Has_Completion (Spec_Id);
      Install_Declarations (Spec_Id);

      Expand_Atomic_Body_Declarations (N, Spec_Id);

      Last_E := Last_Entity (Spec_Id);

      Analyze_Declarations (Declarations (N));

      --  For visibility purposes, all entities in the body are private. Set
      --  First_Private_Entity accordingly, if there was no private part in the
      --  protected declaration.

      if No (First_Private_Entity (Spec_Id)) then
         if Present (Last_E) then
            Set_First_Private_Entity (Spec_Id, Next_Entity (Last_E));
         else
            Set_First_Private_Entity (Spec_Id, First_Entity (Spec_Id));
         end if;
      end if;

      Check_Completion (Body_Id);
      Check_References (Spec_Id);
      Process_End_Label (N, 't', Ref_Id);
      End_Scope;
   end Analyze_Atomic_Body;

   -------------------------------
   -- Analyze_Atomic_Definition --
   -------------------------------

   procedure Analyze_Atomic_Definition (N : Node_Id) is
      E : Entity_Id;
      L : Entity_Id;

      procedure Undelay_Itypes (T : Entity_Id);
      --  Itypes created for the private components of a protected type
      --  do not receive freeze nodes, because there is no scope in which
      --  they can be elaborated, and they can depend on discriminants of
      --  the enclosed protected type. Given that the components can be
      --  composite types with inner components, we traverse recursively
      --  the private components of the protected type, and indicate that
      --  all itypes within are frozen. This ensures that no freeze nodes
      --  will be generated for them.
      --
      --  On the other hand, components of the corresponding record are
      --  frozen (or receive itype references) as for other records.

      --------------------
      -- Undelay_Itypes --
      --------------------

      procedure Undelay_Itypes (T : Entity_Id) is
         Comp : Entity_Id;

      begin
         if Is_Protected_Type (T) then
            Comp := First_Private_Entity (T);
         elsif Is_Record_Type (T) then
            Comp := First_Entity (T);
         else
            return;
         end if;

         while Present (Comp) loop
            if Is_Type (Comp)
              and then Is_Itype (Comp)
            then
               Set_Has_Delayed_Freeze (Comp, False);
               Set_Is_Frozen (Comp);

               if Is_Record_Type (Comp)
                 or else Is_Protected_Type (Comp)
               then
                  Undelay_Itypes (Comp);
               end if;
            end if;

            Next_Entity (Comp);
         end loop;
      end Undelay_Itypes;

   --  Start of processing for Analyze_Atomic_Definition

   begin
      Tasking_Used := True;

      Analyze_Declarations (Visible_Declarations (N));

      if Present (Private_Declarations (N))
        and then not Is_Empty_List (Private_Declarations (N))
      then
         L := Last_Entity (Current_Scope);
         Analyze_Declarations (Private_Declarations (N));

         if Present (L) then
            Set_First_Private_Entity (Current_Scope, Next_Entity (L));
         else
            Set_First_Private_Entity (Current_Scope,
              First_Entity (Current_Scope));
         end if;
      end if;

      E := First_Private_Entity (Current_Scope);
      while Present (E) loop
         if Is_Task_Type (Etype (E))
           or else Has_Task (Etype (E))
         then
            Set_Has_Task (Current_Scope);
         elsif not Is_Protected_Type (Etype (E)) then
            Error_Msg_N
              ("only tasks and protected objects may be declared in the " &
               "private part of an atomic type", N);
         end if;

         Next_Entity (E);
      end loop;

      Undelay_Itypes (Current_Scope);

      Check_Max_Actions (N, Max_Actions);
      Process_End_Label (N, 'e', Current_Scope);
   end Analyze_Atomic_Definition;

   -------------------------------------
   -- Analyze_Atomic_Type_Declaration --
   -------------------------------------

   procedure Analyze_Atomic_Type_Declaration (N : Node_Id) is
      Def_Id : constant Entity_Id := Defining_Identifier (N);
      E      : Entity_Id;
      T      : Entity_Id;
   begin
      if No_Run_Time_Mode then
         Error_Msg_CRT ("atomic type", N);

         if Has_Aspects (N) then
            Analyze_Aspect_Specifications (N, Def_Id);
         end if;

         return;
      end if;

      Tasking_Used := True;
      Check_Restriction (No_Atomic_Types, N);

      T := Find_Type_Name (N);

      --  In the case of an incomplete type, use the full view, unless it's not
      --  present (as can occur for an incomplete view from a limited with).

      if Ekind (T) = E_Incomplete_Type and then Present (Full_View (T)) then
         T := Full_View (T);
         Set_Completion_Referenced (T);
      end if;

      Set_Ekind              (T, E_Atomic_Type);
      Set_Is_First_Subtype   (T, True);
      Init_Size_Align        (T);
      Set_Etype              (T, T);
      Set_Has_Delayed_Freeze (T, True);
      Set_Stored_Constraint  (T, No_Elist);
      Push_Scope (T);

      --  Check_Interfaces (N, T);

      if Present (Discriminant_Specifications (N)) then
         if Has_Discriminants (T) then

            --  Install discriminants. Also, verify conformance of
            --  discriminants of previous and current view. ???

            Install_Declarations (T);
         else
            Process_Discriminants (N);
         end if;
      end if;

      Set_Is_Constrained (T, not Has_Discriminants (T));

      --  If aspects are present, analyze them now. They can make references
      --  to the discriminants of the type, but not to any components.

      if Has_Aspects (N) then
         Analyze_Aspect_Specifications (N, Def_Id);
      end if;

      Analyze (Atomic_Definition (N));

      --  The Ekind of components is E_Void during analysis to detect illegal
      --  uses. Now it can be set correctly.

      E := First_Entity (Current_Scope);
      while Present (E) loop
         if Ekind (E) = E_Void then
            Set_Ekind (E, E_Component);
            Init_Component_Location (E);
         end if;

         Next_Entity (E);
      end loop;

      End_Scope;

      --  Case of a completion of a private declaration

      if T /= Def_Id
        and then Is_Private_Type (Def_Id)
      then
         --  Deal with preelaborable initialization. Note that this processing
         --  is done by Process_Full_View, but as can be seen below, in this
         --  case the call to Process_Full_View is skipped if any serious
         --  errors have occurred, and we don't want to lose this check.

         if Known_To_Have_Preelab_Init (Def_Id) then
            Set_Must_Have_Preelab_Init (T);
         end if;

         --  Create corresponding record now, because some private dependents
         --  may be subtypes of the partial view.

         --  Skip if errors are present, to prevent cascaded messages

         if Serious_Errors_Detected = 0

           --  Also skip if expander is not active

           and then Full_Expander_Active
         then
            Expand_N_Atomic_Type_Declaration (N);
            Process_Full_View (N, T, Def_Id);
         end if;
      end if;
   end Analyze_Atomic_Type_Declaration;

   ---------------------------------------
   -- Analyze_Single_Atomic_Declaration --
   ---------------------------------------

   procedure Analyze_Single_Atomic_Declaration (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Id     : constant Node_Id    := Defining_Identifier (N);
      T      : Entity_Id;
      T_Decl : Node_Id;
      O_Decl : Node_Id;
      O_Name : constant Entity_Id := Id;

   begin
      Generate_Definition (Id);
      Tasking_Used := True;

      --  The node is rewritten as an atomic type declaration, in exact
      --  analogy with what is done with single tasks.

      T :=
        Make_Defining_Identifier (Sloc (Id),
          New_External_Name (Chars (Id), 'T'));

      T_Decl :=
        Make_Atomic_Type_Declaration (Loc,
         Defining_Identifier => T,
         Atomic_Definition => Relocate_Node (Atomic_Definition (N)));

      O_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => O_Name,
          Object_Definition   => Make_Identifier (Loc,  Chars (T)));

      Rewrite (N, T_Decl);
      Insert_After (N, O_Decl);
      Mark_Rewrite_Insertion (O_Decl);

      --  Enter names of type and object before analysis, because the name of
      --  the object may be used in its own body.

      Enter_Name (T);
      Set_Ekind (T, E_Atomic_Type);
      Set_Etype (T, T);

      Enter_Name (O_Name);
      Set_Ekind (O_Name, E_Variable);
      Set_Etype (O_Name, T);

      --  Instead of calling Analyze on the new node, call the proper analysis
      --  procedure directly. Otherwise the node would be expanded twice, with
      --  disastrous result.

      Analyze_Atomic_Type_Declaration (N);
   end Analyze_Single_Atomic_Declaration;

   -----------------------
   -- Check_Max_Actions --
   -----------------------

   procedure Check_Max_Actions (D : Node_Id; R : All_Parameter_Restrictions) is
      Ecount : Uint;
      Decl   : Node_Id;

   begin
      Ecount := Uint_0;
      Decl := First (Visible_Declarations (D));
      while Present (Decl) loop
         if Nkind (Decl) = N_Action_Declaration then
            Ecount := Ecount + 1;
         end if;

         Next (Decl);
      end loop;
      if Ecount > 0 then
         Check_Restriction (R, D, Ecount);
      end if;
   end Check_Max_Actions;

   --------------------------
   -- Find_Concurrent_Spec --
   --------------------------

   function Find_Concurrent_Spec (Body_Id : Entity_Id) return Entity_Id is
      Spec_Id : Entity_Id := Current_Entity_In_Scope (Body_Id);

   begin
      --  The type may have been given by an incomplete type declaration.
      --  Find full view now.

      if Present (Spec_Id) and then Ekind (Spec_Id) = E_Incomplete_Type then
         Spec_Id := Full_View (Spec_Id);
      end if;

      return Spec_Id;
   end Find_Concurrent_Spec;

   --------------------------
   -- Install_Declarations --
   --------------------------

   procedure Install_Declarations (Spec : Entity_Id) is
      E    : Entity_Id;
      Prev : Entity_Id;
   begin
      E := First_Entity (Spec);
      while Present (E) loop
         Prev := Current_Entity (E);
         if Prev /= E then
            Set_Current_Entity (E);
            Set_Is_Immediately_Visible (E);
            Set_Homonym (E, Prev);
         end if;
         Next_Entity (E);
      end loop;
   end Install_Declarations;

end Sem_Atom;
