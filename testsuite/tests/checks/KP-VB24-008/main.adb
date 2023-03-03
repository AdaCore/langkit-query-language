procedure Main is
   type Bytes is array (Natural range <>) of Integer;
   type Bytes_Ptr is access Bytes;

   procedure Test (Ctx : out Natural; Buffer : in out Bytes_Ptr)
   with
     Post =>  --  FLAG
       Buffer = null and
       Ctx = Buffer'First'Old
   is
   begin
      Ctx := Buffer'First;
      Buffer := null;
   end;

   Buffer : Bytes_Ptr := new Bytes'(0, 0, 0);
   Ctx    : Natural;
begin
   Test (Ctx, Buffer);
end;
