package P is

   type Root is interface;
   type Parent is interface and Root;
   type Som is interface and Parent;

   procedure Check (Object : Som'Class);

end P;
