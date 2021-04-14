package Exception_Declarations is

   procedure Proc_1;
   procedure Proc_2;
   procedure Proc_3;

   generic function Fun (I : Integer) return Integer;

   task T is
      entry E;
   end T;

end Exception_Declarations;
