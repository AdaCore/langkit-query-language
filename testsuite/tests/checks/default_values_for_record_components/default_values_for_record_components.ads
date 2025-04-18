package Default_Values_For_Record_Components is
   type Rec (D : Natural := 0) is record   --  NOFLAG
      I : Integer := 0;                    --  FLAG
      B : Boolean;

      case D is
         when 0 =>
            C : Character := 'A';          --  FLAG
         when others =>
            F : Float;                     --  NOFLAG
      end case;
   end record;
end Default_Values_For_Record_Components;
