with Types; use Types;

package Exp_Atom is

   type Action_Sub_Kind is
     (Dispatching_Kind,
      External_Kind,
      Internal_Kind);
   --  This type is used to distinguish the different subprograms generated for
   --  an action.

   function Action_Index_Expression
     (Sloc  : Source_Ptr;
      Act   : Entity_Id;
      Atm   : Entity_Id) return Node_Id;
   --  Compute the index position for an action call.

   procedure Expand_Action_Body_Statements        (N : Node_Id);
   --  Expand statements of an action body. Currently it just adds a label
   --  at the end of the statements for use when expanding a return statement
   --  inside the action.

   procedure Expand_Atomic_Body_Declarations
     (N       : Node_Id;
      Spec_Id : Entity_Id);
   --  Expand declarations required for an atomic body. See bodies of both
   --  Expand_Atomic_Body_Declarations and Expand_N_Atomic_Body for full
   --  details of the nature and use of these declarations. The second argument
   --  is the entity for the corresponding atomic type declaration.

   procedure Expand_N_Action_Body                 (N : Node_Id);
   procedure Expand_N_Action_Call_Statement       (N : Node_Id);
   procedure Expand_N_Alternative_Action_Select   (N : Node_Id);
   procedure Expand_N_Atomic_Body                 (N : Node_Id);
   procedure Expand_N_Atomic_Type_Declaration     (N : Node_Id);

   function Make_Initialize_Atomic
     (Atomic_Rec : Entity_Id) return List_Id;
   --  Given the entity of the record type created for an atomic type, build
   --  a list of statements needed for proper initialization of the object.

   function Next_Action (N : Node_Id) return Node_Id;
   --  Given an action body node, find the following node in the declarations
   --  list.

end Exp_Atom;
