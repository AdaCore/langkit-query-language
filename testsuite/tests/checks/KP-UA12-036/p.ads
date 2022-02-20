package P is

   type Root_Object is tagged null record;

   type Type1 is array (1..4) of Long_Float;
   for Type1'Alignment use 32;

   type Nasty is new Root_Object with record  -- FLAG
      X : Type1;
   end record;

end P;
