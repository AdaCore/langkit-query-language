with Ada.Exceptions;
with GNAT.Traceback.Symbolic;
with GNAT.Compiler_Version;
with Ada.Text_IO;

with Ada.Containers.Bounded_Vectors;

with Ada.Command_Line;

procedure Bounded_Vector_Return_Issue is

   package V is new
     Ada.Containers.Bounded_Vectors
       (Index_Type   => Positive,
        Element_Type => Integer);
   Size : constant Positive :=
     (if Ada.Command_Line.Argument_Count >= 1
      then Positive'Value (Ada.Command_Line.Argument (1))
      else 10);

   subtype Bounded_Vector is V.Vector (Ada.Containers.Count_Type (Size));
   use type Bounded_Vector;

   package Bounded_Vector_Sort is new V.Generic_Sorting;

   function Sort (X : Bounded_Vector) return Bounded_Vector is
      R : V.Vector := X;
   begin
      Bounded_Vector_Sort.Sort (R);
      return R;
   end Sort;

   procedure Process is

      V : Bounded_Vector;

   begin

      V.Append (1);
      V.Append (2);
      V.Append (3);
      V.Append (4);
      V.Append (5);

      declare
         Middle   : Positive := (Positive (V.Length) - 1) / 2 + 1;
         V_Sorted : Bounded_Vector := Sort (V);
         Median   : Integer := V_Sorted (Middle);  -- NO FLAG
      begin
         Ada.Text_IO.Put_Line ("Median " & Median'Img);
      end;

      declare
         Middle : Positive := (Positive (V.Length) - 1) / 2 + 1;
         Median : Integer := Sort (V) (Middle);  -- FLAG
      begin
         Ada.Text_IO.Put_Line ("Median " & Median'Img);
      end;
   end Process;

   package C_Version is new GNAT.Compiler_Version;
begin
   Ada.Text_IO.Put_Line ("Compiler_Version  :" & C_Version.Version);
   Process;
exception
   when Error : others =>
      Ada.Text_IO.Put_Line
        ("Main :exception raised " & Ada.Exceptions.Exception_Message (Error));
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
      Ada.Text_IO.Put_Line
        (GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
end;
