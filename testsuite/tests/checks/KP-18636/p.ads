package P is

   type Root is interface;
   type One is interface and Root;

   procedure Proc (Object : One'Class);

end P;
