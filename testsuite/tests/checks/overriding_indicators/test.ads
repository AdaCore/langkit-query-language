package Test is
   type T1 is null record;
   procedure Prim1 (Self : T1; Other : Integer);
   function Prim2 (Self : T1; Other : Integer) return String;
   function Prim4 (Self : T1) return Integer is (10);

   type T2 is new T1;
   procedure Prim1 (Self : T2; Other : Integer); -- FLAG
   function Prim2 (Self : T2; Other : Integer) return String; -- FLAG
   procedure Prim3 (Self : T2; Other : Integer) is null; -- NOFLAG
   function Prim4 (Self : T2) return Integer is (12) --  FLAG;

   type T3 is new T1;
   overriding procedure Prim1 (Self : T3; Other : Integer); -- NOFLAG
   overriding function Prim2 (Self : T3; Other : Integer) return String; -- NOFLAG
   overriding function Prim4 (Self : T3) return Integer is (12) --  NOFLAG;

   procedure Not_A_Prim (Self : Integer);  -- NOFLAG

   type T4 is tagged null record;
   procedure Prim1 (Self : T4; Other : Integer);
   function Prim2 (Self : T4; Other : Integer) return String;
   procedure Prim3 (Self : T4; Other : Integer);

   type T5 is new T4 with null record;
   overriding procedure Prim1 (Self : T5; Other : Integer); -- NOFLAG
   overriding function Prim2 (Self : T5; Other : Integer) return String; -- NOFLAG

   type T6 is new T4 with null record;
   procedure Prim1 (Self : T6; Other : Integer);  -- FLAG
   function Prim2 (Self : T6; Other : Integer) return String; -- FLAG
   procedure Prim3 (Self : T6; Other : Integer);  -- FLAG

end Test;
