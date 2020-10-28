package body Iters.Adapters is

   ------------------
   -- Wrapped_Iter --
   ------------------

   function Wrapped_Iter
     (Iter : Filter_Adapter) return Wrapped_Iters.Iterator_Access
   is
      (Iter.Wrapped);

   ----------
   -- Next --
   ----------

   function Next (Iter    : in out Filter_Adapter;
                             Result  : out Wrapped_Iters.Element_Type)
                             return Boolean
   is
      Element : Wrapped_Iters.Element_Type;
   begin
      if Iter.Wrapped.Next (Element)
        and then Iter.Filter_Fn (Iter.Wrapped, Element)
      then
         Result := Element;
         return True;
      else
         return False;
      end if;
   end Next;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Iter : in out Filter_Adapter) is
   begin
      Wrapped_Iters.Release_Access (Iter.Wrapped);
   end Release;

   -----------
   -- Clone --
   -----------

   overriding function Clone (Iter : Filter_Adapter) return Filter_Adapter is
      Wrapped_Clone : constant Wrapped_Iters.Iterator_Access :=
        new Wrapped_Iters.Iterator_Interface'Class'
          (Wrapped_Iters.Iterator_Interface'Class (Iter.Wrapped.Clone));
   begin
      return Filter_Adapter'(Wrapped_Clone, Iter.Filter_Fn);
   end Clone;

   -------------------------
   -- Make_Filter_Adapter --
   -------------------------

   function Make_Filter_Adapter (Iter : Wrapped_Iters.Iterator_Access;
                                 Fn   : Filter_Func_Access)
                                 return Filter_Adapter
   is
      (Filter_Adapter'(Iter, Fn));

end Iters.Adapters;
