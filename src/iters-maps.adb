package body Iters.Maps is

   ----------
   -- Next --
   ----------

   overriding function Next (Iter   : in out Map_Iter;
                             Result : out Result_Type) return Boolean
   is
      Input_Element : Input_Iterators.Element_Type;
      Input_Exists  : constant Boolean := Iter.Inner.Next (Input_Element);
   begin
      if not Input_Exists then
         return False;
      end if;

      Result := Iter.Fn.Evaluate (Input_Element);
      return True;
   end Next;

   -----------
   -- Clone --
   -----------

   overriding function Clone (Iter : Map_Iter) return Map_Iter is
      Fn_Copy : constant Map_Funcs.Func_Access :=
        new Map_Funcs.Func'Class'(Map_Funcs.Func'Class (Iter.Fn.Clone));
      Inner_Copy : constant Input_Iterators.Iterator_Access :=
        new Input_Iterators.Iterator_Interface'Class'
          (Input_Iterators.Iterator_Interface'Class (Iter.Inner.Clone));
   begin
      return Map_Iter'(Inner_Copy, Fn_Copy);
   end Clone;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Iter : in out Map_Iter) is
   begin
      Map_Funcs.Release_Access (Iter.Fn);
      Input_Iterators.Release_Access (Iter.Inner);
   end Release;

   ---------
   -- Map --
   ---------

   function Map (Input : Input_Iterators.Iterator_Access;
                 Fn    : Map_Funcs.Func_Access) return Map_Iter
   is
   begin
      return Map_Iter'(Input, Fn);
   end Map;

   ---------
   -- Map --
   ---------

   function Map (Input : Input_Iterators.Iterator_Interface'Class;
                 Fn    : Map_Funcs.Func'Class) return Map_Iter
   is
      Input_Ptr : constant Input_Iterators.Iterator_Access :=
        new Input_Iterators.Iterator_Interface'Class'(Input);
      Fn_Ptr    : constant Map_Funcs.Func_Access :=
        new Map_Funcs.Func'Class'(Fn);
   begin
      return Map_Iter'(Input_Ptr, Fn_Ptr);
   end Map;

end Iters.Maps;
