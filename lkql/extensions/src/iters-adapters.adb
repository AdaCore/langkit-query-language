------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
--                                                                          --
-- LKQL is free software;  you can redistribute it and/or modify  it        --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

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
