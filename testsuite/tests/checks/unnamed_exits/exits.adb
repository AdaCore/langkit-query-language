procedure Exits is
begin
   Named: loop
      loop
         exit;    --  NO FLAG
      end loop;

      exit;       --  FLAG
      exit Named; --  NO FLAG
   end loop Named;
end Exits;
