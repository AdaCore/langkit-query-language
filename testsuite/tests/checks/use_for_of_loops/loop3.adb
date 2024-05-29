with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
    type Int_Array is array (Natural range <>) of Integer;
    type Int_List (Length, Other : Natural := 10) is record
        Dep_Content : Int_Array (1 .. Length);
        Other_Content : Int_Array (1 .. Other);
        Undep_Content : Int_Array (1 .. 10);
    end record;

    type Cond_Int_List (Cond : Boolean := True) is record
       Undep_Content : Int_Array (1 .. 10);
       case Cond is
          when True =>
             Content : Int_Array (1 .. 10);
          when False => null;
       end case;
   end record;

    procedure With_In_Out_Param (L : in out Int_List) is
        Copy : Int_List := L;
        Renamed : Int_List renames L;
        Const : constant Int_List := L;
    begin
        for I in L.Dep_Content'Range loop        --  NOFLAG
            Put_Line (L.Dep_Content (I)'Image);
        end loop;
        for I in L.Other_Content'Range loop      --  NOFLAG
            Put_Line (L.Other_Content (I)'Image);
        end loop;
        for I in L.Undep_Content'Range loop      --  FLAG
            Put_Line (L.Undep_Content (I)'Image);
        end loop;
        for I in Copy.Dep_Content'Range loop     --  NOFLAG
            Put_Line (Copy.Dep_Content (I)'Image);
        end loop;
        for I in Renamed.Dep_Content'Range loop  --  NOFLAG
            Put_Line (Renamed.Dep_Content (I)'Image);
        end loop;
        for I in Const.Dep_Content'Range loop    --  FLAG
            Put_Line (Const.Dep_Content (I)'Image);
        end loop;
    end With_In_Out_Param;

    procedure With_Out_Param (L : out Int_List) is
        Copy : Int_List := L;
    begin
        for I in L.Dep_Content'Range loop        --  NOFLAG
            Put_Line (L.Dep_Content (I)'Image);
        end loop;
        for I in Copy.Dep_Content'Range loop     --  NOFLAG
            Put_Line (Copy.Dep_Content (I)'Image);
        end loop;
    end With_Out_Param;

    procedure With_In_Param (L : in Int_List) is
        Copy : Int_List := L;
    begin
        for I in L.Dep_Content'Range loop        --  FLAG
            Put_Line (L.Dep_Content (I)'Image);
        end loop;
        for I in Copy.Dep_Content'Range loop     --  NOFLAG
            Put_Line (Copy.Dep_Content (I)'Image);
        end loop;
    end With_In_Param;

    procedure Cond_Component (L : in out Cond_Int_List) is
    begin
        for I in L.Content'Range loop            --  NOFLAG
            Put_Line (L.Content (I)'Image);
        end loop;
        for I in L.Undep_Content'Range loop      --  FLAG
            Put_Line (L.Undep_Content (I)'Image);
        end loop;
    end Cond_Component;
begin
    null;
end Main;
