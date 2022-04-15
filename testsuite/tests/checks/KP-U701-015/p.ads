package P is

   type Odd is new Natural
      with Dynamic_Predicate => Odd mod 2 = 1,
           Predicate_Failure => "Value not odd =" &   --  FLAG
                                             Natural'Image(Natural(Odd));

   subtype Small_Odd is Odd
      with Static_Predicate => Small_Odd < 100,
          Predicate_Failure => "Value not small =" &   --  FLAG
                                             Natural'Image(Natural(Small_Odd));

end P;

