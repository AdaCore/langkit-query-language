with LKQL.Primitives;    use LKQL.Primitives;

private package LKQL.Patterns is

   use Primitive_Options;

   type Match_Result is tagged record
      Matched_Value  : Primitive_Option;
      --  True if the matching attempt succeeded
   end record;
   --  Represents that result of a matching attempt

   function Make_Match_Success
     (Matched_Value : Primitive)
      return Match_Result;
   --  Create a Match_Result value representing a successful matching attempt
   --  with the given binding(s).

   function Is_Success (Self : Match_Result) return Boolean;
   --  Return whether the match was successful

   function Get_Matched_Value (Self : Match_Result) return Primitive
     with Pre => Self.Is_Success;
   --  If the match was successful, return the matched value.
   --  Otherwise, raise an exception.

   Match_Failure : constant Match_Result :=
     Match_Result'(Matched_Value => None);
   --  Special value representing the failure of a matching attempt

   subtype Match_Index is Integer range Positive'First - 1 .. Positive'Last;

   type Match_Array_Result is record
      Matched_Value : Primitive_Option;
      --  If the match was successful, stores the matched value
      Index    :  Match_Index := Match_Index'First;
      --  Index of the first matched pattern
   end record;
   --  Represents the result of a matching attempt against a sequence of
   --  patterns.

end LKQL.Patterns;
