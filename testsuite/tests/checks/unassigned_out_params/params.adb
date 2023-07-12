procedure Params is

   procedure Proc1 (I : out Integer;
                    B, B2 : out Boolean;   --  FLAG B2
                    C :  Integer) is
   begin
     B := True;
     I := C;
   end;

   type Arr is array (1 .. 10) of Integer;

   type Rec is record
      C1 : Arr;
      C2 : Integer;
   end record;

   procedure Proc2                         -- NOFLAG
     (P1 : out Arr;
      P2 : out Rec;
      P3 : out Boolean)
   is
   begin
      P2.C1 (1) := 1;
      Proc1 (P1 (1), P3, P3, 1);
   end;

   procedure Proc2_Exceptions
     (P1 : out Arr;
      P2 : out Rec;                        --  FLAG
      P3 : out Boolean)                    --  FLAG
   is
   begin
      P2.C1 (1) := 1;
      Proc1 (P1 (1), P3, P3, 1);
   exception
      when Constraint_Error =>             --  P2 & P3 not set
         P1 (1) := 0;
      when others =>                       --  P3 not set
         P1 (1) := 1;
         P2.C2  := 2;
   end;

   type Access_Arr is access Arr;

   procedure Proc3
     (P1, P2, P3 : out Access_Arr;         --  FLAG P2
      P4         : out Integer;            --  FLAG P4
      P5         : out Arr)
   is
   begin
      P1 := new Arr;
      P2.all (1) := 1;
      P3 (1) := 1;    --  Note that strictly speaking this is equivalent to
                      --  the line above but this check isn't required to
                      --  detect implicit dereferences so will consider that
                      --  P3 is assigned.
      P5 (P4) := 2;
   end;

   procedure Proc (X : out Integer) is     -- FLAG X
   begin
      begin
         null;
      exception
         when others => X := 1;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         begin
            null;
         exception
            when others =>
               x := 3;
         end;
   end Proc;

   procedure Proc_With_Exception_Handler
     (A,                                   --  FLAG A
      B : out Integer) is
   begin
      B := 1;
   exception
      when others =>
         raise;
   end;

begin
   null;
end Params;
