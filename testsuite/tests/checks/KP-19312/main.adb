with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Text_IO;           use Ada.Text_IO;

procedure Main is

   type Int_Array is array (1 .. 10) of Integer;

   type Iterator is null record with Iterable =>
     (First => First,
      Next => Next,
      Has_Element => Has_Element,
      Element => Element);

   function First (Ignore : Iterator) return Integer is (1);

   function Next (Ignore   : Iterator;
                  Position : Integer) return Integer is
     (Position + 1);

   function Has_Element (Ignore   : Iterator;
                         Position : Integer) return Boolean is
     (Position <= 3);

   function Element (Ignore   : Iterator;
                     Position : Integer) return Integer is
     (0);

   package Integer_Sets is new
     Ada.Containers.Ordered_Sets
       (Element_Type => Integer);

   I : constant Iterator := (null record);
   A : constant Int_Array := (others => 0);
   S_1 : constant Integer_Sets.Set := [for E of I when False => E];  --  FLAG
   S_2 : constant Integer_Sets.Set := [for E of I => E];             --  NOFLAG

begin
   for Elem in I loop                   --  NOFLAG
      Put_Line ("Elem: " & Elem'Image);
   end loop;

   for Elem of I loop                   --  NOFLAG
      Put_Line ("Elem: " & Elem'Image);
   end loop;

   for Elem in I when False loop        --  FLAG
      Put_Line ("Elem: " & Elem'Image);
   end loop;

   for Elem of I when False loop        --  FLAG
      Put_Line ("Elem: " & Elem'Image);
   end loop;

   for Elem in A'range when False loop  --  NOFLAG
      Put_Line ("Elem: " & Elem'Image);
   end loop;

   for Elem of A when False loop        --  NOFLAG
      Put_Line ("Elem: " & Elem'Image);
   end loop;
end Main;
