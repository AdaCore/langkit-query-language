package Deeply_Nested_Generics is
   generic
   package P_G is
   end;

   generic
   package P_G_0 is -- FLAG (depth=0)
      generic procedure P;   --  FLAG (depth=1)

      generic
        with package Formal is new P_G; -- NO_FLAG
      package P_G_1 is --  FLAG (depth=1)
         generic
         package P_G_2 is -- FLAG (depth=2)
            I  : Integer;
         end;
      end;
   end;

end Deeply_Nested_Generics;
