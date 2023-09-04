procedure Proc2 is

    type Enum is (A, B, C);
    Var1 : Enum := B;
    Var2 : Enum := C;

    Bool : Boolean;

    function Junk (R : Enum) return Boolean is (R = A);
    function Junk (L, R : Enum) return Boolean is (L <= R);
begin

    User_Defoned_NOT: declare
       function "not" (R : Enum) return Boolean is (R > A);
    begin
       Bool := not (not Var1);          --  NOFLAG
    end User_Defoned_NOT;

    Renamed_NOT: declare
       function "not" (R : Enum) return Boolean renames Junk;
    begin
       Bool := not (not Var1);          --  NOFLAG
    end Renamed_NOT;

    User_Defined_Relation: declare
       function ">" (L, R : Enum) return Boolean is (R >= R);
    begin
       Bool := not (Var1 > Var2);       --  NOFLAG
    end User_Defined_Relation;

    Renamed_Relation: declare
       function ">" (L, R : Enum) return Boolean renames Junk;
    begin
       Bool := not (Var1 > Var2);       --  NOFLAG
    end Renamed_Relation;

    Bool := not (not Bool);             --  FLAG
    Bool := Standard."not" (not Bool);  --  NOFLAG
    Bool := "not" (not Bool);           --  NOFLAG

    Bool := not (Var1 > Var2);          --  FLAG
    Bool := not (">" (Var1, A));        --  NOFLAG

end Proc2;
