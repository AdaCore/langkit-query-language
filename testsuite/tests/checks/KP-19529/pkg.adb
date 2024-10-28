package body Pkg is
   function Create (I : Integer; B : Boolean) return P_Rec is
   begin
      return P_Rec'(I, B);
   end Create;
end Pkg;
