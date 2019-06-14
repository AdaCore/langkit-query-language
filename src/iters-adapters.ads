with Iters.Iterators;

generic

   with package Wrapped_Iters is new Iters.Iterators (<>);

package Iters.Adapters is

   type Filter_Func_Access is access
     function (Iter  : in out Wrapped_Iters.Iterator_Access;
               Value : Wrapped_Iters.Element_Type)
               return Boolean;

   type Filter_Adapter is new Wrapped_Iters.Iterator_Interface with private;

   overriding function Next (Iter    : in out Filter_Adapter;
                             Result  : out Wrapped_Iters.Element_Type)
                             return Boolean;

   overriding function Clone (Iter : Filter_Adapter) return Filter_Adapter;

   overriding procedure Release (Iter : in out Filter_Adapter);

   function Wrapped_Iter
     (Iter : Filter_Adapter) return Wrapped_Iters.Iterator_Access;

   function Make_Filter_Adapter (Iter : Wrapped_Iters.Iterator_Access;
                                 Fn   : Filter_Func_Access)
                                 return Filter_Adapter;

private

   type Filter_Adapter is new Wrapped_Iters.Iterator_Interface with record
      Wrapped   : Wrapped_Iters.Iterator_Access;
      Filter_Fn : Filter_Func_Access;
   end record;

end Iters.Adapters;
