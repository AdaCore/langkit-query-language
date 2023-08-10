procedure Exits is
begin
   Named: loop
      loop
         exit;    --  NOFLAG
      end loop;

      exit;       --  FLAG
      exit Named; --  NOFLAG
   end loop Named;
end Exits;
