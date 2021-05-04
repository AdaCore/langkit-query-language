package Enumeration_Ranges_In_Case_Statements is
   type Enum is (A, B, C, D, E, F, G);

   function F (J : Integer) return Enum;

   procedure Bar (I : in out Integer);
end Enumeration_Ranges_In_Case_Statements;
