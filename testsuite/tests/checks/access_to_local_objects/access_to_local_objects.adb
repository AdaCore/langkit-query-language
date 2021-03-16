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

    task T;
    task body T is
       type Int_A is access all Integer;
       Var4 : aliased Integer;
       Var5 : Int_A := Var4'Access;   -- FLAG
    begin
       null;
    end T;

    protected Pro is
       entry E;
    end Pro;

    protected body Pro is

       entry E when True is
           type Int_A is access all Integer;
           Local : aliased Integer;
           P : Int_A := Local'Access; -- FLAG
       begin null;
       end E;

    end Pro;

end Access_To_Local_Objects;
