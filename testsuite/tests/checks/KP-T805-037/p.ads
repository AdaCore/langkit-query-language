package P is

   subtype Index_Type is Integer range 0 .. 45;

   type Data  is record
      Set_Index : Integer := 4;
   end record;

   Nil_Data : constant Data := (Set_Index => 4);

   type Data_Array is array (Index_Type range <>) of Data;

   type Data_Record (Count : Index_Type := 0) is record
      Data : Data_Array (1 .. Count);
   end record;

   type Kind is (A);

   type Wrong  --  FLAG
     (K   : Kind := A;
      Num : Index_Type := Index_Type'First) is
   record
      case K is
         when A =>
            Field : Data_Record (Num) :=
               (Count => Num,
                Data  => (others => Nil_Data));
      end case;
   end record;

end P;
