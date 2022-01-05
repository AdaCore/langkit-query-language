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
