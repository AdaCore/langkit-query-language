package Parents is

   type I1 is limited interface;
   type I2 is limited interface;
   type I3 is limited interface;

   --  tagged type
   type T_Root is tagged null record;

   type T_1 is new T_Root with null record;
   type T_2 is new T_Root and I1 with null record;
   type T_3 is new T_Root and I1 and I2 with null record;   --  FLAG
   type T_4 is new T_Root and I1 and I2 and I3 with null record;   --  FLAG

   --  protected type
   protected type PT is new I1 and I2 and I3 with  --  FLAG
      entry E;
   end PT;

   --  single protected declaration
   protected P01 is new I1 and I2 and I3 with --  FLAG
      entry E;
   end P01;

   --  task type declaration
   task type TT1 is new I1 and I2 and I3 with  --  FLAG
      entry E;
   end TT1;

   --  single task declaration
   task TO1 is new I1 and I2 and I3 with  --  FLAG
      entry E;
   end TO1;

end Parents;
