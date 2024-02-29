with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure cal is

   year: Integer;
   firstDay: String(1..10);
   lang: String(1..7);
   response: Boolean;
   daysInMonth: Integer;

   type DaysArray is array(0..6) of String(1..10); -- Adjusted length to accommodate longest day name
   days: constant DaysArray := (
      0 => "Sunday    ",    -- Adjusted to 10 characters
      1 => "Monday    ",   -- Adjusted to 10 characters
      2 => "Tuesday   ",   -- Adjusted to 10 characters
      3 => "Wednesday ",   -- Adjusted to 10 characters
      4 => "Thursday  ",   -- Adjusted to 10 characters
      5 => "Friday    ",   -- Adjusted to 10 characters
      6 => "Saturday  "   -- Adjusted to 10 characters
   ); 

   function isYearValid(year: Integer) return Boolean is
   begin
      -- Check if the inputted year is valid
      return year >= 1582;
   end isYearValid;

   procedure readCalInfo(
      year     : out Integer;
      firstDay : out String;
      lang     : out String
   ) is
      inputYear : Integer;
      calcYear : Integer;
      dayForFirstIndex: Integer;
      validInputYear: Boolean := False;
   begin
      while not validInputYear loop
         put_Line("Enter a year greater than 1582 (inclusive):");
         get(inputYear);
         put_line("");
         
         validInputYear := isYearValid(inputYear);

         if not validInputYear then
            put_Line("Please ensure that your entered year is a valid integer and greater than 1582 (inclusive).");
            put_line("");
         else
            put_Line(trim(Integer'Image(inputYear), side => ada.strings.Left) & " is a valid year!");
         end if;
      end loop;

      -- Calculate the day that will be the first of the month
      calcYear := inputYear - 1;
      dayForFirstIndex := (36 + calcYear + (calcYear / 4) - (calcYear / 100) + (calcYear / 400)) mod 7; 
      firstDay := days(dayForFirstIndex);
      
      year := inputYear;  -- Assigning the valid year with :=
      lang := "English";  -- Placeholder value with :=
   end readCalInfo;

   function leapYear(year: Integer) return Boolean is
   begin
      --divisible by 4 for non-century = leap year
      --divisible by 400 for century = leap year

      if year mod 4 = 0 then
         return True;
      elsif year mod 400 = 0 then
         return True;
      end if;

      return False;
   end leapYear;

   function numDaysInMonth(month: Integer; year: Integer) return Integer is
      leapYearResponse: Boolean;
   begin

      case month is
         when 0 | 2 | 4 | 6 | 7 | 9 | 11 =>
            return 31;
         when 3 | 5 | 8 | 10 =>
            return 30;
         when 1 =>

            -- Check for leap year
            leapYearResponse := leapYear(year);

            if leapYearResponse then
               return 29;
            else
               return 28;
            end if;
         when others =>
            return 0; -- Invalid month
      end case;
   end numDaysInMonth;

   
begin
   -- Test readCalInfo
   readCalInfo(year, firstDay, lang);
   put_Line("Year: " & Integer'Image(year) & ", FirstDay: " & firstDay & ", Lang: " & lang);
   response := leapYear(year);
   put_Line("Leap Year? : " & Boolean'Image(response));
   daysInMonth := numDaysInMonth(1, year);
   put_line("Days in February: " & Integer'Image(daysInMonth));
   daysInMonth := numDaysInMonth(6, year);
   put_line("Days in July: " & Integer'Image(daysInMonth));
   daysInMonth := numDaysInMonth(7, year);
   put_line("Days in August: " & Integer'Image(daysInMonth));
   daysInMonth := numDaysInMonth(11, year);
   put_line("Days in December: " & Integer'Image(daysInMonth));
end cal;
