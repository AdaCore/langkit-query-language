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

with Ada.Strings;
with Ada.Wide_Wide_Characters.Unicode; use Ada.Wide_Wide_Characters.Unicode;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with System.Address_Image;

package body LKQL.String_Utils is

   function Make_Underlining (Left_Padding     : Natural;
                              Underlining      : Natural;
                              Right_Padding    : Natural)
                              return Unbounded_Text_Type;

   ----------------------
   -- Make_Underlining --
   ----------------------

   function Make_Underlining (Left_Padding     : Natural;
                              Underlining      : Natural;
                              Right_Padding    : Natural)
                              return Unbounded_Text_Type
   is
   begin
      return (Left_Padding    * ' ') &
             (Underlining     * '^') &
             (Right_Padding   * ' ');
   end Make_Underlining;

   -----------------
   -- Split_Lines --
   -----------------

   function Split_Lines (Str : Text_Type) return String_Vectors.Vector is
      Start  : Positive := Str'First;
      Result : String_Vectors.Vector;
   begin
      for I in Str'First .. Str'Last + 1 loop
         if I = Str'Last + 1 or else Is_Line_Terminator (Str (I)) then
            Result.Append (To_Unbounded_Text (Str (Start .. I - 1)));
            Start := I + 1;
         end if;
      end loop;

      return Result;
   end Split_Lines;

   ---------------------
   -- Underline_Range --
   ---------------------

   function Underline_Range (Line  : Unbounded_Text_Type;
                             Start : Positive;
                             Stop  : Positive) return Unbounded_Text_Type
   is
      use Langkit_Support.Text.Chars;
      Actual_Start  : constant Integer :=
        Integer'Max (Start, Index_Non_Blank (Line));
      Actual_Stop   : constant Integer :=
        Integer'Min (Stop, Index_Non_Blank (Line, Ada.Strings.Backward));
      Left          : constant Natural :=
        Integer'Max (0, Actual_Start - 1);
      Underlining   : constant Natural := Actual_Stop - Actual_Start + 1;
      Right         : constant Natural := Length (Line) - Actual_Stop;
   begin
      if Actual_Stop <= Actual_Start then
         return Line;
      end if;
      return Line & LF & Make_Underlining (Left, Underlining, Right) & LF;
   end Underline_Range;

   --------------------
   -- Underline_From --
   --------------------

   function Underline_From (Line  : Unbounded_Text_Type;
                            Start : Positive) return Unbounded_Text_Type
   is
      Stop  : constant Integer := Index_Non_Blank (Line, Ada.Strings.Backward);
   begin
      return Underline_Range (Line, Start, Stop);
   end Underline_From;

   ------------------
   -- Underline_To --
   ------------------

   function Underline_To   (Line : Unbounded_Text_Type;
                            Stop : Positive) return Unbounded_Text_Type
   is
      Start : constant Integer := Index_Non_Blank (Line);
   begin
      return Underline_Range (Line, Start, Stop);
   end Underline_To;

   ---------------
   -- Underline --
   ---------------

   function Underline
     (Line : Unbounded_Text_Type) return Unbounded_Text_Type
   is
      Start : constant Integer := Index_Non_Blank (Line);
      Stop  : constant Integer := Index_Non_Blank (Line, Ada.Strings.Backward);
   begin
      return (if Start < 1 or else Stop < 1 or else Stop <= Start then Line
              else Underline_Range (Line, Start, Stop));
   end Underline;

   -------------------
   -- Address_Image --
   -------------------

   function Address_Image (Addr : System.Address) return String is
      Ret : constant String := System.Address_Image (Addr);
      I : Natural := 0;
   begin
      for C of Ret loop
         exit when C /= '0';
         I := I + 1;
      end loop;

      return To_Lower (Ret (Natural'Min (Ret'Last, I + 1) .. Ret'Last));
   end Address_Image;

end LKQL.String_Utils;
