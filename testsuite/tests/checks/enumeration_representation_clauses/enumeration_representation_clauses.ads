package Enumeration_Representation_Clauses is
   type Enum1 is (A1, B1, C1);
   for Enum1 use (A1 => 1, B1 => 11, C1 => 111);     --  FLAG
end Enumeration_Representation_Clauses;
