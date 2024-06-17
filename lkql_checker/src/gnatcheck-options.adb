with Gnatcheck.Output; use Gnatcheck.Output;

package body Gnatcheck.Options is
   procedure Warning (Self : in out Gnatcheck_Error_Handler; Msg : String) is
   begin
      Warning (Msg);
   end Warning;

   procedure Error (Self : in out Gnatcheck_Error_Handler; Msg : String) is
   begin
      Error (Msg);
   end Error;

end Gnatcheck.Options;
