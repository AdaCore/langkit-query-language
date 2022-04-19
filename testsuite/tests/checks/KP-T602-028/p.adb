procedure P is
   package Nested is
      protected type DT is
         procedure PPN1 (F : Float);
         procedure PPN1 (I : Integer);
      private
         Data : Integer := 1234;
      end DT;
   end Nested;
   use Nested;

   package body Nested is
      protected body DT is
         procedure PPN1 (F : Float) is
         begin
            null;
         end;

         procedure PPN1 (I : Integer) is
         begin
            null;
         end;
      end DT;
   end;

   procedure Do_Test (P : access protected procedure (V : Integer)) is
   begin
      P.all (42);
   end;

   Obj : DT;
begin
   Do_Test (Obj.PPN1'Access);  --  FLAG
end;
