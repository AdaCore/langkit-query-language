with Gnatcheck.Output; use Gnatcheck.Output;

with System.Multiprocessors;

package body Gnatcheck.Options is
   procedure Warning (Self : in out Gnatcheck_Error_Handler; Msg : String) is
   begin
      Warning (Msg);
   end Warning;

   procedure Error (Self : in out Gnatcheck_Error_Handler; Msg : String) is
   begin
      Error (Msg);
   end Error;

   ------------------
   -- Jobs_Convert --
   ------------------

   function Jobs_Convert (Arg : String) return Natural is
      Value : constant Natural := Natural'Value (Arg);
   begin
      if Value = 0 then
         return Natural (System.Multiprocessors.Number_Of_CPUs);
      else
         return Value;
      end if;
   end Jobs_Convert;

end Gnatcheck.Options;
