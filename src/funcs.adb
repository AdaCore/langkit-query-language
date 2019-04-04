package body Funcs is

   --------------------
   -- Release_Access --
   --------------------

   procedure Release_Access (F : in out Func_Access) is
   begin
      if F /= null then
         F.Release;
         Free_Func (F);
      end if;
   end Release_Access;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (Self    : in out Ada_Func_Wrapper;
                      Element : Argument_Type) return Return_Type
   is
   begin
      return Self.Fn (Element);
   end Evaluate;

   -----------
   -- Clone --
   -----------

   function Clone (Self : Ada_Func_Wrapper) return Ada_Func_Wrapper is
   begin
      return Ada_Func_Wrapper'(Fn => Self.Fn);
   end Clone;

   -------------
   -- To_Func --
   -------------

   function To_Func (Fn : Ada_Func_Access) return Ada_Func_Wrapper is
   begin
      return Ada_Func_Wrapper'(Fn => Fn);
   end To_Func;

end Funcs;
