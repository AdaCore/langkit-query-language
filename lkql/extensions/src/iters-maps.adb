------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
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
