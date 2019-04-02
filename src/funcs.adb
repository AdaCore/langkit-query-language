package body Funcs is

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
