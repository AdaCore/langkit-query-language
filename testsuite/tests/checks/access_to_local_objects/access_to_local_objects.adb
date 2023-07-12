with Generic_Int;
with Generic_Parent;

package body Access_To_Local_Objects is
    package Global_Int_Pkg is new Generic_Int;

    Global : aliased Integer;

    procedure P is
       type Int_A is access all Integer;
       Var1 : aliased Integer;
       Var2 :         Int_A := Var1'Access;  --  FLAG
       Var3 :         Int_A := Global'Access;  -- NOFLAG

       type Procedure_Access is access procedure (Input : aliased in out Integer);
       procedure P_Local (Input : aliased in out Integer) is
          type Int_A_P is access all Integer;
          Local_Var : Int_A_P := Input'Access; -- FLAG
       begin null;
       end;
       Var4 : Procedure_Access := P_Local'Access; -- NOFLAG

       Var5 : Int_A := Var3.all'Access;  -- NOFLAG

       Global_Renamed : Integer renames Global;
       Var7 : Int_A := Global_Renamed'Access;  -- NOFLAG
       Global_Renamed2 : Integer renames Global_Renamed;
       Var8 : Int_A := Global_Renamed2'Access;  -- NOFLAG

       package Local_Int_Pkg is new Generic_Int;
       Var9 : Int_A := Global_Int_Pkg.Int'Access; -- NOFLAG
       Var10 : Int_A := Local_Int_Pkg.Int'Access; -- FLAG

       package Local_Nested_Int_Pkg is new Generic_Parent;
       Var11 : Int_A := Local_Nested_Int_Pkg.Child.Int'Access; -- FLAG
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

    procedure Local_Records is
       type Local_Record_T is record
           I : aliased Integer;
       end record;
       type Local_Record_A_T is access all Local_Record_T;
       type Int_A is access all Integer;
       Local_Record : aliased Local_Record_T;
       Local_Record_A : Local_Record_A_T := Local_Record'Access; -- FLAG
       Local_Record_Comp : Int_A := Local_Record.I'Access; -- FLAG
       Local_Record_A_Comp : Int_A := Local_Record_A.I'Access; -- NOFLAG
    begin null;
    end Local_Records;

end Access_To_Local_Objects;
