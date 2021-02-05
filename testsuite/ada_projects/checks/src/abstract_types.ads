package Abstract_Types is

   type Figure is abstract tagged private;              --  FLAG
   
private
   type Figure is abstract tagged null record;          --  FLAG
   
   type SubFigure is new Figure with null record;
end Abstract_Types;
