package body Main is
   procedure Main (T : Tag_T_Acc) is
      I_Sub : Sub_Integer := 42;
   begin
      T.all.Proc (I_Sub);  -- NOFLAG
   end Main;

   package body Gen_Pkg is
      procedure Main (T : Tag_T_Acc) is
         I     : T_Integer := 42;
         I_Sub : Sub_Integer := 42;
         I_Der : Sub_Integer := 42;
         S     : Sub_String := "Hello";
      begin
         T.all.Proc (I_Sub);             -- FLAG
         T.all.Proc (I_Der);             -- FLAG
         T.all.Proc (I);                 -- NOFLAG
         T.all.Proc_Not_Out (I_Sub);     -- NOFLAG
         T.all.Proc_Str (S);             -- NOFLAG
         T.all.Proc_No_Precond (I_Sub);  -- NOFLAG
      end Main;
   end Gen_Pkg;

   package body Gen_Pkg_Inner is
      procedure Proc (T : Inner_Tag_T; I : out G_Int) is
      begin
         null;
      end Proc;

      procedure Proc_Same_Type (T : Inner_Tag_T; I : out G_Int) is
      begin
         null;
      end Proc_Same_Type;

      procedure Main (T : Inner_Tag_T_Acc) is
         I   : G_Int := 42;
         S_I : Sub_G_Int := I;
      begin
         T.all.Proc (S_I);          -- FLAG
         T.all.Proc_Same_Type (I);  -- NOFLAG
      end Main;
   end Gen_Pkg_Inner;

   package Inst is new Gen_Pkg;
   package Inst_Inner_Int is new Gen_Pkg_Inner (Sub_Integer);
   package Inst_No_Subp is new Gen_Pkg_No_Subp;
end Main;
