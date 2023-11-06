with Ada.Command_Line;
with Ada.Text_IO;

procedure Main is
   X : Integer;
begin
   X := Integer'Value(Ada.Command_Line.Argument(1));
   if X > 50 then
      X := 50;
      goto Label0; -- NOFLAG: directly contained within if/elsif/else
   end if;

   if X > 25 then
      X := 25;
   else
      null;
      goto Label0; -- NOFLAG: directly contained within if/elsif/else
   end if;

   case X is
      when 24 => goto Label0; -- NOFLAG: directly contained within case/when
      when others =>
         X := X / 2;
         goto Label1; -- NOFLAG: directly contained within case/when
   end case;

   <<Label1>>
   loop
      null;
      if X < 12 then
         X := 12;
      end if;

      goto Label2; -- FLAG: not contained within if
   end loop;

   <<Label2>>
   if X > 12 then
      loop
         null;
         if X > 18 then
            X := 18;
         end if;

         goto Label0; -- FLAG: not _directly_ contained within if
      end loop;
   end if;

   <<Label0>>
   Ada.Text_IO.Put_Line(Integer'Image(X));
end Main;
