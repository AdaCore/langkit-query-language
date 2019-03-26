generic

   type Value_Type is private;

package Options is

   type Option_Kind is (Kind_None, Kind_Some);

   type Option (Kind : Option_Kind := Kind_None) is private;

   function Is_Some (Self : Option) return Boolean;

   function Is_None (Self : Option) return Boolean;

   function Extract (Self : Option) return Value_Type;

   function To_Option (Value : Value_Type) return Option;

   function None return Option;

private

   type Option (Kind : Option_Kind := Kind_None) is record
      case Kind is
         when Kind_None =>
            null;
         when Kind_Some =>
            Value : Value_Type;
      end case;
   end record;

end Options;
