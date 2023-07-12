with Ada.Tags; use Ada.Tags;

procedure P is
   T : Tag := No_Tag;

   type TT is tagged null record;
begin
   T := Descendant_Tag ("foo", T);   --  FLAG
   T := Descendant_Tag ("foo", TT'Tag);   -- NOFLAG

   if Is_Descendant_At_Same_Level (No_Tag, T) then   --  FLAG
      declare
         A : constant Tag_Array := Interface_Ancestor_Tags (No_Tag); --  FLAG
         B : constant Tag_Array := Interface_Ancestor_Tags (TT'Tag); -- NOFLAG
      begin
         null;
      end;
   end if;
end P;
