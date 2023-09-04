package body Class is

   function Test (X : NT1) return Boolean is
   begin
      return False;
   end Test;

   procedure Proc (X : in out NT1) is null;  --  NOFLAG
   procedure Proc (X : in out NT2) is        --  NOFLAG
   begin
      null;
   end Proc;

   type NT3 is new T and Int1 with null record;
   procedure Proc (X : in out NT3) is        --  FLAG
   begin
      null;
   end Proc;

end Class;
