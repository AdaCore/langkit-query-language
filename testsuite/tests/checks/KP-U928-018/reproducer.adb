with Section; use Section;
with Common; use Common;

procedure Reproducer is
   Current_Section : Object := Empty_Section;
   Lang            : constant Language_Type := Language_Type'("Ada");
   Pattern         : constant Pattern_Type := Pattern_Type' ("OK");
begin
   Current_Section.Add_Excluded_Language_Pattern (Lang, Pattern);
   for Excl_Patt of Excluded_Patterns (Current_Section) (Lang) loop  --  FLAG
      null;
   end loop;
end Reproducer;
