package body Access_To_Local_Objects is

    Var0 : aliased Integer;

    procedure P is
       type Int_A is access all Integer;
       Var1 : aliased Integer;
       Var2 :         Int_A := Var1'Access;  --  FLAG
       Var3 :         Int_A := Var0'Access;  --  NO FLAG
    begin
        null;
    end P;

end Access_To_Local_Objects;
