with Ada.Unchecked_Deallocation;
generic

   type Argument_Type (<>) is private;
   --  Function's argument type

   type Return_Type (<>) is private;
   --  Function's return type

package Funcs is

   type Func is interface;
   --  Abstraction representing a function that takes Element_Type values
   --  and returns values of type Return_Type.

   type Func_Access is access all Func'Class;
   --  Pointer to a Func

   function Evaluate (Self    : in out Func;
                      Element : Argument_Type)
                      return Return_Type is abstract;
   --  Apply the current Func to Element

   function Clone (Self : Func) return Func is abstract;
   --  Perform a deep copy of the given Func

   procedure Release (Self : in out Func) is null;
   --  Release ressources that belong to Self

   procedure Release_Access (F : in out Func_Access);
   --  Release the resources of the function that is accessed through 'F',
   --  the free 'F' itself.

   procedure Free_Func is new Ada.Unchecked_Deallocation
     (Func'Class, Func_Access);
   --  Free the memory accessed through a Func_Access pointer

   type Ada_Func_Access is access
     function (X : Argument_Type) return Return_Type;
   --  Pointer to an Ada function that takes an Element_Type value and
   --  returns a Return_Type value.

   type Ada_Func_Wrapper is new Func with private;

   function Evaluate (Self    : in out Ada_Func_Wrapper;
                      Element : Argument_Type) return Return_Type;

   function Clone (Self : Ada_Func_Wrapper) return Ada_Func_Wrapper;

   function To_Func (Fn : Ada_Func_Access) return Ada_Func_Wrapper;

private

   type Ada_Func_Wrapper is new Func with record
      Fn : Ada_Func_Access;
   end record;

end Funcs;
