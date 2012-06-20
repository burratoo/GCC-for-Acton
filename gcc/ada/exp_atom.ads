with Types; use Types;

package Exp_Atom is

   procedure Expand_Atomic_Body_Declarations
     (N       : Node_Id;
      Spec_Id : Entity_Id);
   --  Expand declarations required for an atomic body. See bodies of both
   --  Expand_Atomic_Body_Declarations and Expand_N_Atomic_Body for full
   --  details of the nature and use of these declarations. The second argument
   --  is the entity for the corresponding atomic type declaration.

   procedure Expand_N_Atomic_Type_Declaration (N : Node_Id);

end Exp_Atom;
