procedure P is
   type T is tagged record
      Field1 : aliased Integer;
   end record;
   type T_Ptr is access all T;

   function Get return T_Ptr with Import;
   procedure Register (X : access Integer) with Import;

   Mylw  : T_Ptr := Get;
   Thelw : T renames mylw.all;

   Mylwx  : T_Ptr := Get;
   Thelwx : T renames T (Mylwx.all);

   type T2 is record
      Field1 : aliased Integer;
   end record;
   type T2_Ptr is access all T;

   function Get return T2_Ptr with Import;

   A : T2_Ptr := Get;
   B : T2 renames T2 (A.all);

begin
   Thelw.Field1 := 0;
   Register (Thelw.Field1'Access);    --  NO FLAG
   Register (Thelwx.Field1'Access);   --  FLAG
   Mylwx := Thelwx'Access;            --  FLAG

   Register (B.Field1'Access);        --  NO FLAG
end P;
