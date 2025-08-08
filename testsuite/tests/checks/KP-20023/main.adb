procedure Main is
   package Root_Pkg is
      type Root is tagged record
         X : Integer;
      end record;
      function Make return Root'Class
      is (Root'(X => 1));
      function Make_Dispatch return Root
      is (X => 0);
   end Root_Pkg;
   use Root_Pkg;

   package Child_Pkg is
      type Child is new Root with null record;
      overriding
      function Make_Dispatch return Child
      is (X => -1);
   end Child_Pkg;

   subtype Sub_Root is Root'Class;
   subtype Sub_Sub_Root is Sub_Root;

   X : Root'Class := Child_Pkg.Child'(X => 1);
   Y : Sub_Root := Child_Pkg.Child'(X => 1);
   Z : Sub_Sub_Root := Child_Pkg.Child'(X => 1);

   Not_Class : Root := Root'(X => 1);

   Cond : Boolean := False;
begin
   X := (if Cond then Make_Dispatch else Make);          -- FLAG
   X := (if Cond then Make else Make_Dispatch);          -- FLAG
   X := ((((if Cond then Make_Dispatch else Make))));    -- FLAG
   X :=                                                  -- FLAG
     (case Cond is
        when True => Make_Dispatch,
        when False => Make);
   Y := (if Cond then Make_Dispatch else Make);          -- FLAG
   Z := (if Cond then Make_Dispatch else Make);          -- FLAG
   X := (if Cond then Make else Make);                   -- NOFLAG
   X := Make_Dispatch;                                   -- NOFLAG
   Not_Class := (if Cond then Make_Dispatch else Make);  -- NOFLAG
end Main;
