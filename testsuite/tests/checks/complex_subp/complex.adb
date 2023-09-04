with Ada.Unchecked_Conversion;

package body Complex is

   procedure Swap (I, J : in out Integer) is   --  FLAG
   begin
      if I /= J then
         declare
            Tmp : Integer;
         begin
            TMP := I;
            I   := J;
            J   := Tmp;
         end;
      end if;
   end Swap;

   procedure Has_Decl with Inline is   --  FLAG
      procedure Nested is
      begin
         null;
      end Nested;
   begin
      null;
   end Has_Decl;

   procedure Many_Stmts;
   pragma Inline (Many_Stmts);

   procedure Many_Stmts is   --  FLAG
      X : Integer;
   begin
      X := 1;
      X := 1;
      X := 1;
      X := 1;
      X := 1;
      X := 1;
   end Many_Stmts;

   procedure With_UC with Inline is   --  NOFLAG
      function Convert is new Ada.Unchecked_Conversion (Integer, Character);
   begin
      null;
   end With_UC;

   generic
   procedure Gen_Proc;

   procedure Gen_Proc is
   begin
      if True then
         null;
      end if;
   end Gen_Proc;

   procedure Inst_Proc is new Gen_Proc with Inline;   --  FLAG

end Complex;
