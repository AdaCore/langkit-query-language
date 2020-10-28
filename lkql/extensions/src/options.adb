package body Options is

   -------------
   -- Is_Some --
   -------------

   function Is_Some (Self : Option) return Boolean is (Self.Kind = Kind_Some);

   -------------
   -- Is_None --
   -------------

   function Is_None (Self : Option) return Boolean is (Self.Kind = Kind_None);

   -------------
   -- Extract --
   -------------

   function Extract (Self : Option) return Value_Type is (Self.Value);

   ---------------
   -- To_Option --
   ---------------

   function To_Option (Value : Value_Type) return Option is
      (Option'(Kind_Some, Value));

   ----------
   -- None --
   ----------

   function None return Option is (Option'(Kind => Kind_None));

end Options;
