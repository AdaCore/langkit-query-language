package Test_Specific_Pre_Post is
   type T is tagged private;

   function Foo (A : Integer) return Boolean;

   function Check1 (X : T) return Boolean;
   function Check2 (X : T) return Boolean;

   procedure Proc1 (X : in out T)           --  FLAG
      with Pre => Check1 (X);

   procedure Proc2 (X : in out T)           --  FLAG
      with Post => Check2 (X);

   function Fun1 (X : T) return Integer     --  FLAG
      with Pre  => Check1 (X),
           Post => Check2 (X);

   function Fun2 (X : T) return Integer
      with Pre'Class  => Check1 (X),
           Post'Class => Check2 (X);

   function Fun3 (X : T) return Integer     --  FLAG
      with Pre'Class  => Check1 (X),
           Post'Class => Check2 (X),
           Pre        => Check1 (X),
           Post       => Check2 (X);
end Test_Specific_Pre_Post;
