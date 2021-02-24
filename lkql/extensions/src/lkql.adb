with Langkit_Support.Text; use Langkit_Support.Text;

package body LKQL is
   ---------------
   -- Node_Text --
   ---------------

   function Node_Text (Self : L.LKQL_Node'Class) return String is
   begin
      return Image (L.Text (Self));
   end Node_Text;

   ------------
   -- Symbol --
   ------------

   function Symbol (Node : L.Identifier) return Symbol_Type is
   begin
      return (if Node.Is_Null
              then null
              else LCO.Get_Symbol (Node.Token_Start));
   end Symbol;

end LKQL;
