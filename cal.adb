with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Exceptions; use Ada.Exceptions;

procedure cal is

   year: Integer;
   firstDay: Integer;
   lang: String(1..7);

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

   type MonthsArray is array(0..11) of String(1..24); -- Adjusted length to accommodate longest month name
   monthNames: constant MonthsArray := (
      0  => "      January           ",   -- Adjusted to 24 characters
      1  => "      February          ",   -- Adjusted to 24 characters
      2  => "       March            ",   -- Adjusted to 24 characters
      3  => "       April            ",   -- Adjusted to 24 characters
      4  => "        May             ",   -- Adjusted to 24 characters
      5  => "       June             ",   -- Adjusted to 24 characters
      6  => "       July             ",   -- Adjusted to 24 characters
      7  => "      August            ",   -- Adjusted to 24 characters
      8  => "     September          ",   -- Adjusted to 24 characters
      9  => "      October           ",   -- Adjusted to 24 characters
      10 => "     November           ",   -- Adjusted to 24 characters
      11 => "     December           "    -- Adjusted to 24 characters
   );

   
   type Digit_Array is array (1..9, 1..6) of Character;
   type Banner_Digits is array (0 .. 9) of Digit_Array;


   --2D arrays representing the months
   type Matrix is array (Integer range 0..5, Integer range 0..6) of Integer;
   type ArrayOfMatrices is array (0..11) of Matrix;
   months : ArrayOfMatrices;

   -- Check if the inputted year is valid
   function isYearValid(year: Integer) return Boolean is
   begin
      return year >= 1582;
   end isYearValid;

   -- Read calendar information from the user
   procedure readCalInfo(
      year     : out Integer;
      firstDay : out Integer;
      lang     : out String
   ) is
      inputYear : Integer;
      calcYear : Integer;
      dayForFirstIndex: Integer;
      validInputYear: Boolean := False;

      -- Custom exception for invalid input
      Invalid_Input : exception;
   begin
      while not validInputYear loop
         put_Line("Enter a year greater than 1582 (inclusive):");
         begin
            get(inputYear);
            put_line("");
            
            validInputYear := isYearValid(inputYear);

            if not validInputYear then
               raise Invalid_Input;
            else
               put_Line(trim(Integer'Image(inputYear), side => ada.strings.Left) & " is a valid year!");
            end if;
         exception
            when Invalid_Input =>
               put_Line("Please ensure that your entered year is a valid integer and greater than 1582 (inclusive).");
               put_line("");
         end;

      end loop;

      -- Calculate the day that will be the first of the month
      calcYear := inputYear - 1;
      dayForFirstIndex := (36 + calcYear + (calcYear / 4) - (calcYear / 100) + (calcYear / 400)) mod 7; 
      firstDay := dayForFirstIndex;
      
      year := inputYear;  -- Assigning the valid year with :=
      lang := "English";  -- Placeholder value with :=
   exception
      when others =>
         put_Line("error printing out the requested year!");
         null;
   end readCalInfo;


   -- Determine if a given year is a leap year
   function leapYear(year: Integer) return Boolean is
   begin
      -- Divisible by 4 for non-century = leap year
      -- Divisible by 400 for century = leap year

      if year mod 4 = 0 then
         return True;
      elsif year mod 400 = 0 then
         return True;
      end if;

      return False;
   end leapYear;

   -- Get the number of days in a month for a given year
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

   -- Build the matrix representation of the months
   procedure buildMonths(year: in Integer; firstDay: in Integer) is
      monthFirstDay: Integer;
      dayNum: Integer := 1;
   begin

      -- Decrease by one in order to set it up for loop
      monthFirstDay := firstDay;

      -- Loop through all the months
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

   -- Print the row heading of the calendar
   procedure printRowHeading(startMonth: in Integer; endMonth: in Integer) is
      daysHeading: String := " Su Mo Tu We Th Fr Sa  ";
   begin
      for i in startMonth..endMonth loop
         put(monthNames(i));
      end loop;
      put_Line("");
      for i in startMonth..endMonth loop
         put(daysHeading);
      end loop;
   end printRowHeading;

   -- Print the rows of each month
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

      -- Printing the rows of each month
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

   -- Load font data for printing numbers
   procedure buildCalendar(year: in Integer; firstDay: in Integer; months: ArrayOfMatrices) is
      type SingleNumber is array(0..9) of String(1..7);
      type FontArray is array(0..9) of SingleNumber;

      -- Load font data from file
      procedure loadFont(theArray: in out FontArray) is
         infp: file_type;
         numCounter: integer;
         lineNum: integer;
      begin
         numCounter := 0;
         lineNum := 0;
         open(infp, in_file, "numberFont.dat");
         loop
            exit when end_of_file(infp);
            get(infp, theArray(numCounter)(lineNum));
            lineNum := lineNum + 1;
            if (lineNum = 10) then
                  numCounter := numCounter + 1;
                  lineNum := 0;
            end if;
         end loop;
         close(infp);
      end loadFont;

      fontData: FontArray;

      digitOne, digitTwo, digitThree, digitFour: Integer;

   begin

      loadFont(fontData);

      digitOne := year / 1000;
      digitTwo := (year mod 1000) / 100;
      digitThree := (year mod 100) / 10;
      digitFour := year mod 10;

      -- Print out a number (for example, number 2)
      -- This will print the number represented by the font in the console
      for i in 0..9 loop
         put("                ");
         put(fontData(digitOne)(i) & "  "); put(fontData(digitTwo)(i) & "  "); put(fontData(digitThree)(i) & "  "); put(fontData(digitFour)(i));
         put("                ");
         put_line(""); -- Print newline after each row of the number
      end loop;

      put_Line("");

      buildMonths(year, firstDay);
      printRowHeading(0, 2);
      put_Line("");
      printRowMonth(months, 0, 2);
      put_Line("");
      printRowHeading(3, 5);
      put_Line("");
      printRowMonth(months, 3, 5);
      put_Line("");
      printRowHeading(6, 8);
      put_Line("");
      printRowMonth(months, 6, 8);
      put_Line("");
      printRowHeading(9, 11);
      put_Line("");
      printRowMonth(months, 9, 11);

   end buildCalendar;

   
begin
   -- Test readCalInfo
   readCalInfo(year, firstDay, lang);
   put_Line("");
   buildCalendar(year, firstDay, months);
   put_Line("");
end cal;
