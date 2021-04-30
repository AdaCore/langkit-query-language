with Recursive_Subprograms.Generics; use Recursive_Subprograms.Generics;
package Recursive_Subprograms.In_Instance is

   package Simple_Recursion_Cases_Instance is new Simple_Recursion_Cases;

   package Recursion_And_Default_Component_Initialization_Instance is
     new Recursion_And_Default_Component_Initialization;

   package Recursion_And_Default_Parameter_Initialization_Instance is
      new Recursion_And_Default_Parameter_Initialization;

end Recursive_Subprograms.In_Instance;
