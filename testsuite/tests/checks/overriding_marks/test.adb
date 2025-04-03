package body Test is
   procedure Prim1 (Self : T1; Other : Integer) is null;
   function Prim2 (Self : T1; Other : Integer) return String
   is
   begin
      return "hello";
   end Prim2;

   procedure Prim1 (Self : T2; Other : Integer) is null; -- FLAG
   function Prim2 (Self : T2; Other : Integer) return String
   is
   begin
      return "hello";
   end Prim2; -- FLAG

   overriding procedure Prim1 (Self : T3; Other : Integer) is null; -- NOFLAG
   overriding function Prim2 (Self : T3; Other : Integer) return String
   is
   begin
      return "hello";
   end Prim2; -- NOFLAG

   procedure Not_A_Prim (Self : Integer) is null;  -- NOFLAG

   procedure Prim1 (Self : T4; Other : Integer) is null;
   function Prim2 (Self : T4; Other : Integer) return String
   is
   begin
      return "hello";
   end Prim2;

   overriding procedure Prim1 (Self : T5; Other : Integer) is null; -- NOFLAG
   overriding function Prim2 (Self : T5; Other : Integer) return String
   is
   begin
      return "hello";
   end Prim2; -- NOFLAG

   procedure Prim1 (Self : T6; Other : Integer) is null;  -- FLAG
   function Prim2 (Self : T6; Other : Integer) return String
   is
   begin
      return "hello";
   end Prim2; -- FLAG

   procedure Prim3 (Self : T4; Other : Integer) is null;

   procedure Prim3 (Self : T6; Other : Integer) is separate;
end Test;
