package Types is

   type Tagged_Private is tagged private;

   procedure P;

private
   type Tagged_Private is tagged null record;
end Types;
