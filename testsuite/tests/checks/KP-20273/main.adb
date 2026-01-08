pragma Ada_2022;

procedure Main is
   type Class_Settings_Type is record
      C : Boolean;
   end record;

   type Class_Index_Type is range 1 .. 10;

   type Obj_Settings_List_Type is
     array (Class_Index_Type range <>) of Class_Settings_Type;

   Obj_Settings_List : constant Obj_Settings_List_Type := [];

   type Class_Type is record
      C2 : Boolean;
   end record;

   function Create_Obj (Setttings : Class_Settings_Type) return Class_Type is
   begin
      return (C2 => True);
   end Create_Obj;

   Class_List : array (Obj_Settings_List'Range) of Class_Type :=
     [for I in Obj_Settings_List'Range =>  -- FLAG
        Create_Obj (Setttings => Obj_Settings_List (I))];
begin
   null;
end Main;
