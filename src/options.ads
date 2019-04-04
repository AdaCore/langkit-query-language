--  This package provides a generic 'Option' type, meant to represent optional
--  values.

generic

   type Value_Type is private;
   --  Type of the optional value

package Options is

   type Option_Kind is
     (Kind_None,
      --  The Option doesn't contains a value
      Kind_Some
      --  The Option contains a value
     );

   type Option (Kind : Option_Kind := Kind_None) is private;
   --  Stores an optional value

   function Is_Some (Self : Option) return Boolean;
   --  Return True if the option contains a value

   function Is_None (Self : Option) return Boolean;
   --  Return True if the option doesn't contain a value

   function Extract (Self : Option) return Value_Type
     with Pre => Self.Kind = Kind_Some;
   --  Return the wraped value, if any.
   --  An Assertion_Error will be raised if there is no value.

   function To_Option (Value : Value_Type) return Option;
   --  Create an Option value that contains the given value

   function None return Option;
   --  Create an Option that doesn't contain a value

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
