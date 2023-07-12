with Ada.Finalization; use Ada.Finalization;

package P is
   type Access_To_Array is access String;

   type Parent is tagged record
      F0 : Access_To_Array;  -- NOFLAG
   end record;

   type Child is new Parent with record
      F1 : Integer;          -- NOFLAG
      F2 : Access_To_Array;  -- FLAG
      F3 : access String;    -- FLAG
   end record;

   generic
      type T is private;
      type T_Vector is array (Positive range <>) of T;

   package Gen is

      type T_Vector_Access is access all T_Vector;

      type Vector is new Controlled with record
         V : T_Vector_Access := null;   --  FLAG
      end record;
   end Gen;

end;
