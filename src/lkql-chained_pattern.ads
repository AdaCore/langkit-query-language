with Iters.Iterators;
with LKQL.Common;        use LKQL.Common;
with LKQL.Patterns;      use LKQL.Patterns;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;

with Ada.Containers.Hashed_Sets;
with Ada.Containers.Doubly_Linked_Lists;

private package LKQL.Chained_Pattern is

   package Match_Result_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Match_Result);

   subtype Match_Result_List is Match_Result_Lists.List;

   package Chained_Pattern_Iterators is
     new Iters.Iterators (Match_Result);

   subtype Chained_Pattern_Iter
     is Chained_Pattern_Iterators.Iterator_Interface;

   type Chained_Pattern_Iterator is new Chained_Pattern_Iter with private;

   overriding function Next (Iter   : in out Chained_Pattern_Iterator;
                             Result : out Match_Result)
                             return Boolean;

   overriding function Clone (Iter : Chained_Pattern_Iterator)
                              return Chained_Pattern_Iterator;

   overriding procedure Release (Iter : in out Chained_Pattern_Iterator);

   function Make_Chained_Pattern_Iterator (Ctx     : Eval_Context;
                                           Pattern : L.Chained_Node_Pattern)
                                           return Chained_Pattern_Iterator;

private

   package Node_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => LAL.Ada_Node,
      Hash                => LAL.Hash,
      Equivalent_Elements => LAL."=",
      "="                 => LAL."=");

   subtype Node_Set is Node_Sets.Set;

   type Chained_Pattern_Iterator is new Chained_Pattern_Iter with record
      Ctx                    : Eval_Context;
      Next_Values            : Match_Result_List;
      Pattern                : L.Chained_Node_Pattern;
      Root_Elements_Iterator : Node_Iterator_Access;
      Yielded_Elements       : Node_Set;
   end record;

   procedure Eval_Element (Iter : in out Chained_Pattern_Iterator;
                           Root : LAL.Ada_Node);

   procedure Eval_Chain_From (Iter        : in out Chained_Pattern_Iterator;
                              Root        : LAL.Ada_Node;
                              Current_Env : Environment_Map;
                              Link_Nb     : Positive);

   procedure Eval_Chain_From_Link
     (Iter        : in out Chained_Pattern_Iterator;
      Root        : LAL.Ada_Node;
      Current_Env : Environment_Map;
      Link_Nb     : Positive)
     with Pre => Link_Nb <= Iter.Pattern.F_Chain.Children_Count;

end LKQL.Chained_Pattern;
