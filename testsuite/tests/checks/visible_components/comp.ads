with Types;
package Comp is
   type Null_Record is null record;              --  NOFLAG because tagged_only

   type Not_Null_Record is record                --  NOFLAG because tagged_only
      I : Integer;
      B : Boolean;
   end record;

   type Tagged_Not_Null_Record is tagged record  --  FLAG
      I : Integer;
      B : Boolean;
   end record;

   type Private_Extension is new Types.Tagged_Private with private;

   type NoN_Private_Extension is new Types.Tagged_Private with record  --  FLAG
      B : Boolean;
   end record;

private
   type Rec is tagged record
      I : Integer;
   end record;

   type Private_Extension is new Types.Tagged_Private with record
      C : Rec;
   end record;
end Comp;
