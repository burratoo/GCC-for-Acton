with Types; use Types;

package Sem_Atom is

   procedure Analyze_Action_Body                      (N : Node_Id);
   procedure Analyze_Action_Body_Formal_Part          (N : Node_Id);
   procedure Analyze_Action_Call_Alternative          (N : Node_Id);
   procedure Analyze_Action_Declaration               (N : Node_Id);
   procedure Analyze_Alternative_Action_Select        (N : Node_Id);
   procedure Analyze_Atomic_Body                      (N : Node_Id);
   procedure Analyze_Atomic_Definition                (N : Node_Id);
   procedure Analyze_Atomic_Type_Declaration          (N : Node_Id);
   procedure Analyze_Single_Atomic_Declaration        (N : Node_Id);

end Sem_Atom;
