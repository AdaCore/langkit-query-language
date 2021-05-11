------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                        Copyright (C) 2021, AdaCore                       --
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

with System.Storage_Elements;

--  This package provides unbounded holders like the one that will be available
--  in Ada 2020
--  (see http://www.ada-auth.org/standards/2xaarm/html/AA-A-18-32.html).
--  However, we expose more details, in order to be able to use this with
--  incomplete types, despite the very flaky support of Ada for incomplete
--  types.
--
--  Using this for a regular type looks like::
--
--     type Root is tagged null record;
--
--     procedure Foo (Self : Root) is null;
--
--     type Derived is new Root with record
--        A, B : Integer;
--     end record;
--
--     type Derived_2 is new Derived with record
--        D, E : Integer;
--     end record;
--
--     ...
--
--     package Root_Holders is new Unbounded_Holders.Holder
--       (Root'Class, --  Type stored by the holder
--        32          --  Maximum size of the holder
--        );
--
--     Inst : Root_Holders.Holder :=
--       Root_Holders.Create (Derived_2'(1, 2, 3, 4));
--
--     Inst.Unchecked_Get.Foo;
--
--  If you store an object bigger than the size of the holder, you'll get a
--  runtime exception.
--
--  For incomplete types, it will be a bit more complicated. You have to
--  manually instantiate Base_Holders::
--
--     package Root_Holders is new Unbounded_Holders.Base_Holders (32)
--
--  You can then declare the ``Create`` and ``Unchecked_Get`` operations
--  yourself in the spec:
--
--     function Create (Obj : Root'Class) return Root_Holders.Holder;
--     function Unchecked_Get (Holder: Root_Holders.Holder) return Root'Class;
--
--  And then, you'll instantiate the ``Holders_Impl`` yourself in the body, and
--  forward those implementations to your wrapper.

package Unbounded_Holders is

   --  Base holder package. **You should not instantiate this except for the
   --  incomplete type use case**. Instead, use ``Holders``.
   generic
      Max_Size : System.Storage_Elements.Storage_Count;
   package Base_Holders is

      type Unsized_Buffer is array
        (System.Storage_Elements.Storage_Count range <>)
        of System.Storage_Elements.Storage_Element;
      --  Unsized buffer type, used to store the object internally.

      subtype Internal_Buffer is Unsized_Buffer (0 .. Max_Size);
      --  Buffer type sized to ``Max_Size``.

      type Holder is tagged record
         Buffer    : Internal_Buffer;
         --  Buffer in which the objects are stored

         Real_Size : System.Storage_Elements.Storage_Count;
         --  Real size of the stored object. Used to reconstruct the object.
      end record;

   end Base_Holders;

   --  Holder's implementation package. **You should not instantiate this
   --  except for the incomplete type use case**. instead, use ``Holders``.
   generic
      type T (<>) is limited private;
      type T_Access is access all T;
      with package Holders is new Base_Holders (<>);
   package Holders_Impl is
      function Unchecked_Get (Self : Holders.Holder) return T_Access;
      function Create (Value : T) return Holders.Holder;
   end Holders_Impl;

   --  Main Holders package. See toplevel documentation for an example of use.
   generic
      type T (<>) is limited private;
      --  Type that the holder will hold.

      Max_Size : System.Storage_Elements.Storage_Count;
      --  Maximum size of the holder. If you try to store an object bigger than
      --  that, a ``Constraint_Error`` will be raised.
   package Holders is

      type Holder is tagged private;
      --  Main holder type. This object is ``Max_Size + Storage_Count'Size``
      --  big and holds the objects by value.

      type T_Access is access all T;
      --  Access type returned by the ``Unchecked_Get`` function below.

      function Unchecked_Get (Self : Holder) return T_Access;
      --  Return an access to the object stored inside the holder. This is an
      --  unsafe operation, but enough for our needs for now. If we want to
      --  make it safer we can add an Ada 2012 reference type.

      function Create (Value : T) return Holder;
      --  Create a holder from object ``Value``.

   private

      package T_Base_Holders is new Base_Holders (Max_Size);

      package Holders_Implem is new Holders_Impl
        (T, T_Access, T_Base_Holders);

      type Holder is new T_Base_Holders.Holder with null record;
   end Holders;

end Unbounded_Holders;
