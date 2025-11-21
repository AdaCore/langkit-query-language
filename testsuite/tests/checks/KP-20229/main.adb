with Ada.Text_IO; use Ada.Text_IO;
with System.Machine_Code; use System.Machine_Code;

procedure Main is
   procedure P is null;

   -- Flag because it has clobbers, one output variable that is returned.
   function F1 (Var_In : Integer) return Integer is
      Var_Out : Integer;
   begin
      Asm("addl $42, %1" & ASCII.LF & ASCII.HT &  --  FLAG
          "movl %0, %1",
          Outputs => Integer'Asm_Output ("=a", Var_Out),
          Inputs  => Integer'Asm_Input  ("a", Var_In),
          Clobber => "memory");
      null;
      P;
      return Var_Out;
   end;

   -- Do not flag because it has more than one output.
   function F2 (Var_In : Integer) return Integer is
      Var_Out1, Var_Out2 : Integer;
   begin
      Asm("addl $42, %1" & ASCII.LF & ASCII.HT &  --  NOFLAG
          "movl %0, %1",
          Outputs => (Integer'Asm_Output ("=a", Var_Out1),
                      Integer'Asm_Output ("=a", Var_Out2)),
          Inputs  => Integer'Asm_Input  ("a", Var_In),
          Clobber => "memory");
      return Var_Out1;
   end;

   -- Do not flag because it has no clobbers
   function F3 (Var_In : Integer) return Integer is
      Var_Out1, Var_Out2 : Integer;
   begin
      Asm("addl $42, %1" & ASCII.LF & ASCII.HT &  --  NOFLAG
          "movl %0, %1",
          Outputs => Integer'Asm_Output ("=a", Var_Out1),
          Inputs  => Integer'Asm_Input  ("a", Var_In));
      return Var_Out1;
   end;

   -- Do not flag because it doesn't return the sole output variable.
   function F4 (Var_In : Integer) return Integer is
      Var_Out1, Var_Out2 : Integer;
   begin
      Asm("addl $42, %1" & ASCII.LF & ASCII.HT &  --  NOFLAG
          "movl %0, %1",
          Outputs => Integer'Asm_Output ("=a", Var_Out1),
          Inputs  => Integer'Asm_Input  ("a", Var_In));
      return Var_In;
   end;

   -- Flag because it has clobbers, one output variable that is returned.
   function F5 (Var_In : Integer) return Integer is
      Var_Out : Integer;
   begin
      if True
      then
         Asm("addl $42, %1" & ASCII.LF & ASCII.HT &  --  FLAG
             "movl %0, %1",
             Outputs => Integer'Asm_Output ("=a", Var_Out),
             Inputs  => Integer'Asm_Input  ("a", Var_In),
             Clobber => "memory");
      else
         return 4;
      end if;

      return Var_Out;
   end;
begin
   Put_Line(F1 (1)'Image);
end;
