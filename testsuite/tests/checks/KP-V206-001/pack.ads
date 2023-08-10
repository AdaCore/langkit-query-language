with System.Address_To_Access_Conversions;

package Pack is

   type I1 is limited interface;
   type Tag is tagged null record;

   package Conversions is new System.Address_To_Access_Conversions  --  FLAG
     (Object => I1'Class);

   package Conv2 is new System.Address_To_Access_Conversions  --  NOFLAG
     (Object => Tag);

end Pack;
