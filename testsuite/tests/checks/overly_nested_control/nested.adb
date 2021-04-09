procedure Nested (I, J, Idx : in out Integer) is
begin
   if I > 0 then
      for Idx in I .. J loop
         if J < 0 then
            case I is
               when 1 =>
                  if Idx /= 0 then  --  FLAG (if rule parameter is 3)
                     J := J / Idx;
                  end if;
               when others =>
                  J := J + Idx;
            end case;
         end if;
      end loop;
   end if;
end;
