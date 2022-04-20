with Gen;

package P is

   type Rec is record
      Field : Integer := 0;
   end record;

   Null_Value : constant Rec := (Field => 0);

   package Inst is new Gen (Rec, 1, Null_Value);

end P;
