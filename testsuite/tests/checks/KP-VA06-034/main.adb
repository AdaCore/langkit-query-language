with Ada.Strings.Text_Buffers; use Ada.Strings.Text_Buffers;

procedure Main is

   type BBool is new Boolean
      with Put_Image => Put_Image;

   procedure Put_Image (B: in out Root_Buffer_Type'Class; V : BBool);
   procedure Put_Image (B: in out Root_Buffer_Type'Class; V : BBool) is
   begin
      if V then
         B.Put ("coucou");
      else
         B.Put ("uocuoc");
      end if;
   end Put_Image;

   Obj : BBool;

   S1 : constant String := BBool'Image (BBool'(True));   --  FLAG
   S2 : constant String := BBool'Image (Obj);            --  NO FLAG
begin
   null;
end Main;
