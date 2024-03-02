with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure cal is

   year: Integer;
   firstDay: Integer;
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

   --2D arrays representing the months
   type Matrix is array (Integer range 0..5, Integer range 0..6) of Integer;
   type ArrayOfMatrices is array (0..11) of Matrix;
   months : ArrayOfMatrices;

   function isYearValid(year: Integer) return Boolean is
   begin
      -- Check if the inputted year is valid
      return year >= 1582;
   end isYearValid;

   procedure readCalInfo(
      year     : out Integer;
      firstDay : out Integer;
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
      firstDay := dayForFirstIndex;
      
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

   procedure buildMonths(year: in Integer; firstDay: in Integer) is
      monthFirstDay: Integer;
      dayNum: Integer := 1;
   begin

      --decreasae by one in order to set it up for loop
      monthFirstDay := firstDay;

      --loop through all the months
      for i in months'Range loop

         for Row in months(i)'Range(1) loop

            for Col in months(i)'Range(2) loop

               if (Col < monthFirstDay and Row = 0) or dayNum > numDaysInMonth(i, year) then
                  months(i)(Row, Col) := 0;
               else
                  months(i)(Row, Col) := dayNum;
                  dayNum := dayNum + 1;
               end if;

            end loop;

         end loop;

         monthFirstDay := (monthFirstDay + numDaysInMonth(i, year)) mod 7;
         dayNum := 1;

      end loop; 
   end buildMonths;

   procedure printRowMonth(months: ArrayOfMatrices; startMonth: in Integer; endMonth: in Integer) is
   begin
      -- Loop through all the months
      for i in startMonth..endMonth loop
         for j in 0..6 loop
            if months(i)(0, j) < 10 and months(i)(0, j) > 0 then
               put(" ");
               put(Integer'Image(months(i)(0, j)));
            elsif months(i)(0, j) = 0 then
               put("   ");
            else
               put(Integer'Image(months(i)(0, j)));
            end if;
         end loop;
         put("  ");
      end loop;

      put_Line("");

      for i in startMonth..endMonth loop
         for j in 0..6 loop
            if months(i)(1, j) < 10  and months(i)(1, j) > 0 then
               put(" ");
               put(Integer'Image(months(i)(1, j)));
            elsif months(i)(1, j) = 0 then
               put("   ");
            else
               put(Integer'Image(months(i)(1, j)));
            end if;
         end loop;
         put("  ");
      end loop;

      put_Line("");

      for i in startMonth..endMonth loop
         for j in 0..6 loop
            if months(i)(2, j) < 10  and months(i)(2, j) > 0 then
               put(" ");
               put(Integer'Image(months(i)(2, j)));
            elsif months(i)(2, j) = 0 then
               put("   ");
            else
               put(Integer'Image(months(i)(2, j)));
            end if;
         end loop;
         put("  ");
      end loop;

      put_Line("");

      for i in startMonth..endMonth loop
         for j in 0..6 loop
            if months(i)(3, j) < 10 and months(i)(3, j) > 0 then
               put(" ");
               put(Integer'Image(months(i)(3, j)));
            elsif months(i)(3, j) = 0 then
               put("   ");
            else
               put(Integer'Image(months(i)(3, j)));
            end if;
         end loop;
         put("  ");
      end loop;

      put_Line("");

      for i in startMonth..endMonth loop
         for j in 0..6 loop
            if months(i)(4, j) < 10 and months(i)(4, j) > 0 then
               put(" ");
               put(Integer'Image(months(i)(4, j)));
            elsif months(i)(4, j) = 0 then
               put("   ");
            else
               put(Integer'Image(months(i)(4, j)));
            end if;
         end loop;
         put("  ");
      end loop;

      put_Line("");

      for i in startMonth..endMonth loop
         for j in 0..6 loop
            if months(i)(5, j) < 10 and months(i)(5, j) > 0 then
               put(" ");
               put(Integer'Image(months(i)(5, j)));
            elsif months(i)(5, j) = 0 then
               put("   ");
            else
               put(Integer'Image(months(i)(5, j)));
            end if;
         end loop;
         put("  ");
      end loop;

      put_Line("");
   end printRowMonth;



   
begin
   -- Test readCalInfo
   readCalInfo(year, firstDay, lang);
   put_Line("Year: " & Integer'Image(year) & ", FirstDay: " & Integer'Image(firstDay) & ", Lang: " & lang);
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
   buildMonths(year, firstDay);
   printRowMonth(months, 0, 2);
   put_Line("");
   printRowMonth(months, 3, 5);
   put_Line("");
   printRowMonth(months, 6, 8);
   put_Line("");
   printRowMonth(months, 9, 11);
end cal;
