with Ada.Containers.Vectors;
package body Main_Support is
   package Vec is new Ada.Containers.Vectors (Positive, Integer);
   type VP is access Vec.Vector;

   procedure Iterate (V : VP) is
   begin
      for C of V.all loop   --  FLAG
         null;
      end loop;
   end Iterate;

   protected body Prot is
      procedure Foo is
      begin
         Iterate (null);
         Iterate (new Vec.Vector'(Vec.Empty_Vector));
      end;
   end Prot;
end;
