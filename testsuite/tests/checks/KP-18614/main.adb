procedure Main is

   type on_offs is (is_on, is_off);
   subtype uint3 is Integer range 0 .. 7;

   subtype lists_array is Integer range 1 .. 6;
   type lists is array (lists_array) of on_offs;
   for lists'Component_Size use 2;
   for lists'Size use 6 * 2;

   type test_message_data is record
      my_list : lists;
   end record;

   my_record : test_message_data;

   function list_compare_issue return boolean is
   begin
      return my_record.my_list = (1 => is_on, 2..6 => is_off);  --  FLAG
   end list_compare_issue;

   result : boolean := false;

begin
   result := list_compare_issue;
end Main;
