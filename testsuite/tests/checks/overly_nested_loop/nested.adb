procedure Nested (I, J, Idx : in out Integer) is
begin
   loop
      for Idx in I .. J loop
         loop
            if True then              -- NOFLAG
               for K in 1 .. I loop   --  FLAG (if rule parameter is 2)
                  null;
               end loop;
            end if;
         end loop;
      end loop;
   end loop;
end;
