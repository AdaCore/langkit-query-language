------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2023, AdaCore                     --
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

package body Funcs is

   --------------------
   -- Release_Access --
   --------------------

   procedure Release_Access (F : in out Func_Access) is
   begin
      if F /= null then
         F.Release;
         Free_Func (F);
      end if;
   end Release_Access;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (Self    : in out Ada_Func_Wrapper;
                      Element : Argument_Type) return Return_Type
   is
   begin
      return Self.Fn (Element);
   end Evaluate;

   -----------
   -- Clone --
   -----------

   function Clone (Self : Ada_Func_Wrapper) return Ada_Func_Wrapper is
   begin
      return Ada_Func_Wrapper'(Fn => Self.Fn);
   end Clone;

   -------------
   -- To_Func --
   -------------

   function To_Func (Fn : Ada_Func_Access) return Ada_Func_Wrapper is
   begin
      return Ada_Func_Wrapper'(Fn => Fn);
   end To_Func;

end Funcs;
