package body Options is

   -------------
   -- Is_Some --
   -------------

   function Is_Some (Self : Option) return Boolean is
   begin
      return Self.Kind = Kind_Some;
   end Is_Some;

   -------------
   -- Is_None --
   -------------

   function Is_None (Self : Option) return Boolean is
   begin
      return Self.Kind = Kind_None;
   end Is_None;

   -------------
   -- Extract --
   -------------

   function Extract (Self : Option) return Value_Type is
   begin
      case Self.Kind is
         when Kind_Some =>
            return Self.Value;
         when Kind_None =>
            raise Program_Error with
              "Cannot extract a value from a None variant";
      end case;
   end Extract;

   ---------------
   -- To_Option --
   ---------------

   function To_Option (Value : Value_Type) return Option is
   begin
      return (Kind_Some, Value);
   end To_Option;

   ----------
   -- None --
   ----------

   function None return Option is
   begin
      return Option'(Kind => Kind_None);
   end None;

end Options;
