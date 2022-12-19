with Ada.Tags.Generic_Dispatching_Constructor;

package P is

   type Abstract_Type is abstract tagged limited null record;
   function Create (Params : not null access Natural) return Abstract_Type is abstract;

   type Test_Type;
   task type My_Task;

   type Test_Type is new Abstract_Type with record
      A : Natural;
      T : My_Task;
   end record;

   overriding
   function Create (Params : not null access Natural) return Test_Type;

   function Constructor is new Ada.Tags.Generic_Dispatching_Constructor  --  FLAG
     (T           => Abstract_Type,
      Parameters  => Natural,
      Constructor => Create);

   function Constructor2 is new Ada.Tags.Generic_Dispatching_Constructor  --  FLAG
     (Parameters  => Natural,
      Constructor => Create,
      T           => Test_Type);

   type Non_Limited is abstract tagged null record;
   function Create (Params : not null access Natural) return Non_Abstract;

   function Constructor3 is new Ada.Tags.Generic_Dispatching_Constructor  --  NO FLAG
     (T           => Non_Limited,
      Parameters  => Natural,
      Constructor => Create);

   type No_Task is tagged limited null record;
   function Create (Params : not null access Natural) return No_Task;

   type No_Task2 is new No_Task with record
      A : Natural;
   end record;

   function Constructor4 is new Ada.Tags.Generic_Dispatching_Constructor  --  NO FLAG
     (T           => No_Task,
      Parameters  => Natural,
      Constructor => Create);

end P;
