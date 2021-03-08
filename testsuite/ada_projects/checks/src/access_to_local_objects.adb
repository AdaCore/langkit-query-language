package body Access_To_Local_Objects is

    Var0 : aliased Integer;

    type Int_A is access all Integer;
    procedure P is
       Var1 : aliased Integer;
       Var2 :         Int_A := Var1'Access;  --  FLAG
       Var3 :         Int_A := Var0'Access;  --  NO FLAG
    begin
        null;
    end P;

    task T;
    task body T is
       Var4 : aliased Integer;
       Var5 : Int_A := Var4'Access;  -- FLAG
    begin
       null;
    end T;

end Access_To_Local_Objects;
