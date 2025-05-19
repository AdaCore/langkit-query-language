procedure Test is

   Last_Char_0 : Positive;  -- FLAG

   #if Z'Defined then
   pragma Annotate
     (Gnatcheck,
      Exempt_On,
      "Predefined_Numeric_Types",
      "Exemption justification");
   Last_Char_1 : Positive;
   pragma Annotate (Gnatcheck, Exempt_Off, "Predefined_Numeric_Types");

   Last_Char_2 : Positive; --## rule line off Predefined_Numeric_Types ## Exemption justification

   --## rule off Predefined_Numeric_Types ## Exemption justification
   Last_Char_3 : Positive;
   --## rule on Predefined_Numeric_Types
   #end if;
begin
   null;
end;
