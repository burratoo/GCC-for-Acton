pragma Style_Checks (All_Checks);
--  Turn off subprogram body ordering check. Subprograms are in order by RM
--  section rather than alphabetical.

separate (Par)
package body Atom is

   --  Local subprograms, used only in this chapter
   function P_Action_Body                          return Node_Id;
   function P_Action_Body_Formal_Part              return Node_Id;
   function P_Action_Declaration                   return Node_Id;
   function P_Atomic_Definition                    return Node_Id;
   function P_Atomic_Operation_Declaration         return Node_Id;
   function P_Atomic_Operation_Items               return List_Id;

   -------------------------------
   -- 9.x  Atomic (also 10.1.3) --
   -------------------------------

   --  ATOMIC_TYPE_DECLARATION ::=
   --    atomic type DEFINING_IDENTIFIER [KNOWN_DISCRIMINANT_PART]
   --      [ASPECT_SPECIFICATIONS]
   --    is ATOMIC_DEFINITION;

   --  SINGLE_ATOMIC_DECLARATION ::=
   --    atomic DEFINING_IDENTIFIER
   --      [ASPECT_SPECIFICATIONS]
   --    is ATOMIC_DEFINITION;

   --  ATOMIC_BODY ::=
   --    atomic body DEFINING_IDENTIFIER
   --      [ASPECT_SPECIFICATIONS];
   --    is
   --      {ATOMIC_OPERATION_ITEM}
   --    end [atomic_IDENTIFIER];

   --  Atomic_BODY_STUB ::=
   --    atomic body DEFINING_IDENTIFIER is separate;

   --  This routine scans out a atomic declaration, atomic body
   --  or a atomic stub.

   --  The caller has checked that the initial token is ATOMIC and
   --  scanned past it, so Token is set to the following token.

   --  Error recovery: cannot raise Error_Resync

   function P_Atomic return Node_Id is
      Name_Node   : Node_Id;
      Atomic_Node : Node_Id;
      Atomic_Sloc : Source_Ptr;
      Scan_State  : Saved_Scan_State;

   begin
      Push_Scope_Stack;
      Scope.Table (Scope.Last).Etyp := E_Name;
      Scope.Table (Scope.Last).Ecol := Start_Column;
      Scope.Table (Scope.Last).Lreq := False;
      Atomic_Sloc := Prev_Token_Ptr;

      if Token = Tok_Body then
         Scan; -- past BODY
         Name_Node := P_Defining_Identifier (C_Is);
         Scope.Table (Scope.Last).Labl := Name_Node;

         if Token = Tok_Left_Paren then
            Error_Msg_SC ("discriminant part not allowed in atomic body");
            Discard_Junk_List (P_Known_Discriminant_Part_Opt);
         end if;

         TF_Is;

         --  Atomic stub

         if Token = Tok_Separate then
            Scan; -- past SEPARATE
            Atomic_Node := New_Node (N_Atomic_Body_Stub, Atomic_Sloc);
            Set_Defining_Identifier (Atomic_Node, Name_Node);
            TF_Semicolon;
            Pop_Scope_Stack; -- remove unused entry

         --  Atomic body

         else
            Atomic_Node := New_Node (N_Atomic_Body, Atomic_Sloc);
            Set_Defining_Identifier (Atomic_Node, Name_Node);
            Set_Declarations (Atomic_Node, P_Atomic_Operation_Items);
            End_Statements (Atomic_Node);
         end if;

         return Atomic_Node;

      --  Otherwise we must have a atomic declaration

      else
         if Token = Tok_Type then
            Scan; -- past TYPE
            Atomic_Node :=
              New_Node (N_Atomic_Type_Declaration, Atomic_Sloc);
            Name_Node := P_Defining_Identifier (C_Is);
            Set_Defining_Identifier (Atomic_Node, Name_Node);
            Scope.Table (Scope.Last).Labl := Name_Node;
            Set_Discriminant_Specifications
              (Atomic_Node, P_Known_Discriminant_Part_Opt);

         else
            Atomic_Node :=
              New_Node (N_Single_Atomic_Declaration, Atomic_Sloc);
            Name_Node := P_Defining_Identifier (C_Is);
            Set_Defining_Identifier (Atomic_Node, Name_Node);

            if Token = Tok_Left_Paren then
               Error_Msg_SC
                 ("discriminant part not allowed for single atomic");
               Discard_Junk_List (P_Known_Discriminant_Part_Opt);
            end if;

            Scope.Table (Scope.Last).Labl := Name_Node;
         end if;

         P_Aspect_Specifications (Atomic_Node, Semicolon => False);

         --  Check for semicolon not followed by IS, this is something like

         --    atomic type r;

         --  where we want

         --    atomic type r IS END;

         if Token = Tok_Semicolon then
            Save_Scan_State (Scan_State); -- at semicolon
            Scan; -- past semicolon

            if Token /= Tok_Is then
               Restore_Scan_State (Scan_State);
               Error_Msg_SC -- CODEFIX
                 ("missing IS");
               Set_Atomic_Definition (Atomic_Node,
                 Make_Atomic_Definition (Token_Ptr,
                   Visible_Declarations => Empty_List,
                   End_Label            => Empty));

               SIS_Entry_Active := False;
               End_Statements
                 (Atomic_Definition (Atomic_Node), Atomic_Node);
               return Atomic_Node;
            end if;

            Error_Msg_SP -- CODEFIX
              ("|extra ""("" ignored");
         end if;

         T_Is;

         Set_Atomic_Definition (Atomic_Node, P_Atomic_Definition);
         return Atomic_Node;
      end if;
   end P_Atomic;

   -------------------------------------
   -- 9.x  Atomic Type Declaration --
   -------------------------------------

   --  Parsed by P_Atomic (9.x)

   ------------------------------------
   -- 9.x  Single Atomic Declaration --
   ------------------------------------

   --  Parsed by P_Atomic (9.x)

   ----------------------------
   -- 9.x  Atomic Definition --
   ----------------------------

   --  ATOMIC_DEFINITION ::=
   --      {ACTION_DECLARATION}
   --    [private
   --      {ATOMIC_ELEMENT_DECLARATION}]
   --    end [atomic_IDENTIFIER]

   --  ATOMIC_ELEMENT_DECLARATION ::=
   --    COMPONENT_DECLARATION

   --  The caller has already established the scope stack entry

   --  Error recovery: cannot raise Error_Resync

   function P_Atomic_Definition return Node_Id is
      Def_Node  : Node_Id;
      Item_Node : Node_Id;

   begin
      Def_Node := New_Node (N_Atomic_Definition, Token_Ptr);

      --  Get rid of active SIS entry from outer scope. This means we will
      --  miss some nested cases, but it doesn't seem worth the effort. See
      --  discussion in Par for further details

      SIS_Entry_Active := False;

      --  Loop to scan visible declarations (atomic operation declarations)

      Set_Visible_Declarations (Def_Node, New_List);

      loop
         Item_Node := P_Atomic_Operation_Declaration;
         exit when No (Item_Node);
         Append (Item_Node, Visible_Declarations (Def_Node));
      end loop;

      --  Deal with PRIVATE part (including graceful handling of multiple
      --  PRIVATE parts).

      Private_Loop : while Token = Tok_Private loop
         if No (Private_Declarations (Def_Node)) then
            Set_Private_Declarations (Def_Node, New_List);
         else
            Error_Msg_SC ("duplicate private part");
         end if;

         Scan; -- past PRIVATE

         Declaration_Loop : loop
            P_Component_Items (Private_Declarations (Def_Node));
         end loop Declaration_Loop;
      end loop Private_Loop;

      End_Statements (Def_Node);
      return Def_Node;
   end P_Atomic_Definition;

   ------------------------------------------
   -- 9.x  Atomic Operation Declaration --
   ------------------------------------------

   --  ATOMIC_OPERATION_DECLARATION ::=
   --    ACTION_DECLARATION

   --  Error recovery: cannot raise Error_Resync

   --  Note: a pragma can also be returned in this position

   function P_Atomic_Operation_Declaration return Node_Id is
      L : List_Id;
      P : Source_Ptr;

   begin
      --  This loop runs more than once only when a junk declaration
      --  is skipped.

      loop
         if Token = Tok_Pragma then
            return P_Pragma;

         elsif Token = Tok_Action then
            return P_Action_Declaration;

         elsif Token = Tok_Function then
            L := New_List;
            P := Token_Ptr;
            Skip_Declaration (L);
            Error_Msg
              ("functions cannot be declared in atomic types", P);

         elsif Token = Tok_Procedure then
            L := New_List;
            P := Token_Ptr;
            Skip_Declaration (L);
            Error_Msg
              ("procedures cannot be declared in atomic types", P);

         elsif Token = Tok_Identifier then
            L := New_List;
            P := Token_Ptr;
            Skip_Declaration (L);

            if Nkind (First (L)) = N_Object_Declaration then
               Error_Msg
                 ("component must be declared in private part of " &
                  "atomic type", P);
            else
               Error_Msg
                 ("illegal declaration in atomic definition", P);
            end if;

         elsif Token in Token_Class_Declk then
            Error_Msg_SC ("illegal declaration in atomic definition");
            Resync_Past_Semicolon;

            --  Return now to avoid cascaded messages if next declaration
            --  is a valid component declaration.

            return Error;

         else
            return Empty;
         end if;
      end loop;
   end P_Atomic_Operation_Declaration;

   -----------------------------------
   -- 9.x  Atomic Operation Item --
   -----------------------------------

   --  ATOMIC_OPERATION_ITEM ::=
   --    SUBPROGRAM_DECLARATION
   --  | SUBPROGRAM_BODY
   --  | ACTION_BODY
   --  | REPRESENTATION_CLAUSE

   --  This procedure parses and returns a list of atomic operation items

   --  We are not currently permitting representation clauses to appear
   --  as atomic operation items, do we have to rethink this???

   function P_Atomic_Operation_Items return List_Id is
      Item_List : List_Id;

   begin
      Item_List := New_List;

      loop
         if Token = Tok_Action or else Bad_Spelling_Of (Tok_Action) then
            Append (P_Action_Body, Item_List);

         --  If the operation starts with procedure, function, or an overriding
         --  indicator ("overriding" or "not overriding"), parse a subprogram.

         elsif Token = Tok_Function or else Bad_Spelling_Of (Tok_Function)
                 or else
               Token = Tok_Procedure or else Bad_Spelling_Of (Tok_Procedure)
                 or else
               Token = Tok_Overriding or else Bad_Spelling_Of (Tok_Overriding)
                 or else
               Token = Tok_Not or else Bad_Spelling_Of (Tok_Not)
         then
            Append (P_Subprogram (Pf_Decl_Pbod_Pexp), Item_List);

         elsif Token = Tok_Pragma or else Bad_Spelling_Of (Tok_Pragma) then
            P_Pragmas_Opt (Item_List);

         elsif Token = Tok_Private or else Bad_Spelling_Of (Tok_Private) then
            Error_Msg_SC ("PRIVATE not allowed in protected body");
            Scan; -- past PRIVATE

         elsif Token = Tok_Identifier then
            Error_Msg_SC ("all components must be declared in spec!");
            Resync_Past_Semicolon;

         elsif Token in Token_Class_Declk then
            Error_Msg_SC ("this declaration not allowed in protected body");
            Resync_Past_Semicolon;

         else
            exit;
         end if;
      end loop;

      return Item_List;
   end P_Atomic_Operation_Items;

   -----------------------------
   -- 9.y  Action Declaration --
   -----------------------------

   --  ACTION_DECLARATION ::=
   --    action DEFINING_IDENTIFIER PARAMETER_PROFILE
   --       [ASPECT_SPECIFICATIONS];

   --  The caller has checked that the initial token is ACTION

   --  Error recovery: cannot raise Error_Resync

   function P_Action_Declaration return Node_Id is
      Decl_Node  : Node_Id;
      Scan_State : Saved_Scan_State;

   begin

      Decl_Node := New_Node (N_Action_Declaration, Token_Ptr);
      Scan; -- past ACTION

      Set_Defining_Identifier
        (Decl_Node, P_Defining_Identifier (C_Left_Paren_Semicolon));
      Set_Parameter_Specifications (Decl_Node, P_Formal_Part);

      --  Error recovery check for illegal return

      if Token = Tok_Return then
         Error_Msg_SC ("entry cannot have return value!");
         Scan;
         Discard_Junk_Node (P_Subtype_Indication);
      end if;

      P_Aspect_Specifications (Decl_Node);
      return Decl_Node;

   exception
      when Error_Resync =>
         Resync_Past_Semicolon;
         return Error;
   end P_Action_Declaration;

   ----------------------
   -- 9.y  Action Body --
   ----------------------

   --  ACTION_BODY ::=
   --    action DEFINING_IDENTIFIER ACTION_BODY_FORMAL_PART is
   --      DECLARATIVE_PART
   --    begin
   --      HANDLED_SEQUENCE_OF_STATEMENTS
   --    end [action_IDENTIFIER];

   --  The caller has checked that the initial token is ACTION

   --  Error_Recovery: cannot raise Error_Resync

   function P_Action_Body return Node_Id is
      Action_Node      : Node_Id;
      Formal_Part_Node : Node_Id;
      Name_Node        : Node_Id;

   begin
      Push_Scope_Stack;
      Action_Node := New_Node (N_Action_Body, Token_Ptr);
      Scan; -- past ACTION

      Scope.Table (Scope.Last).Ecol := Start_Column;
      Scope.Table (Scope.Last).Lreq := False;
      Scope.Table (Scope.Last).Etyp := E_Name;
      Scope.Table (Scope.Last).Sloc := Token_Ptr;

      Name_Node := P_Defining_Identifier;
      Set_Defining_Identifier (Action_Node, Name_Node);
      Scope.Table (Scope.Last).Labl := Name_Node;

      Formal_Part_Node := P_Action_Body_Formal_Part;
      Set_Entry_Body_Formal_Part (Action_Node, Formal_Part_Node);

      Parse_Decls_Begin_End (Action_Node);
      return Action_Node;
   end P_Action_Body;

   ----------------------------------
   -- 9.y  Action Body Formal Part --
   ----------------------------------

   --  ACTION_BODY_FORMAL_PART ::=
   --    PARAMETER_PROFILE

   --  Error_Recovery: cannot raise Error_Resync

   function P_Action_Body_Formal_Part return Node_Id is
      Fpart_Node : Node_Id;
      Scan_State : Saved_Scan_State;

   begin
      Fpart_Node := New_Node (N_Action_Body_Formal_Part, Token_Ptr);
      Set_Parameter_Specifications (Fpart_Node, P_Parameter_Profile);
      return Fpart_Node;
   end P_Action_Body_Formal_Part;

   ---------------------------------
   -- 9.y  Action Call Statement --
   ---------------------------------

   --  Parsed by P_Name (4.1). Within a select, an action call is parsed
   --  by P_Select_Statement (9.7)

end Atom;
