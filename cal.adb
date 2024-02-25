with Ada.Text_IO; use Ada.Text_IO;

procedure Cal is
   response: Boolean;
   
   function isValid(year: integer) return Boolean is
   begin
      -- check if the inputted year is valid
      if year >= 1582 then
         return True;
      else
         return False;
      end if;
   end isValid;

   procedure readCalInfo(year: in integer)
   
begin
   -- print out calendar
   put_line("Hello world");
   
   -- call the IsValid function and assign the result to Response
   response := IsValid(1582);
   
   -- print the result
   put_line("Is valid: " & Boolean'Image(Response));
end Cal;
