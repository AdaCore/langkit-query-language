package Test is
   type T is record
      I : Integer;
   end record;

   type S is record
      V : Integer;
   end record with Convention => C_Pass_By_Copy;

   type R is record
      Callback : access procedure (Val : S) with Convention => C;  --  FLAG
   end record with Convention => C;

   function Get return access procedure (Val : S);  --  NOFLAG

   type Acc is access procedure (Val : S) with Convention => C;  --  NOFLAG

   function Foo return access procedure (Val : T) with Convention => C;  --  NOGLAG

   function Bar return access procedure (Val : S);  -- NOGLAG

   function Baz return access procedure (Val : T; Vbl : S; Vcl : R) with Convention => C;  --  FLAG
end Test;
