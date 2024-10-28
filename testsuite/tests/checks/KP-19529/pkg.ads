package Pkg is
   type P_Rec is private;

   function Create (I : Integer; B : Boolean) return P_Rec;
private
   type P_Rec is record
      I : Integer;
      B : Boolean;
   end record;
end Pkg;
