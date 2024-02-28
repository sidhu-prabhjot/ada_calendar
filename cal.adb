with ada.Text_IO; use ada.Text_IO;
with ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with ada.Float_Text_IO; use ada.Float_Text_IO;
with ada.Strings.Fixed; use ada.Strings.Fixed;

procedure cal is

   year: Integer;
   firstDay: String(1..9);
   lang: String(1..7);

   type daysArray is array(0..6) of String(1..9);
   days: constant daysArray := (
      0 => "Monday"   & "   ",
      1 => "Tuesday"  & "  ",
      2 => "Wednesday",
      3 => "Thursday" & " ",
      4 => "Friday"   & "    ",
      5 => "Saturday" & " ",
      6 => "Sunday"   & "   "
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

      --calculate the day that will the the first of the month
      calcYear := inputYear - 1;
      dayForFirstIndex := (36 + calcYear + (calcYear / 4) - (calcYear / 100) + (calcYear / 400)) mod 7; 
      firstDay := days(dayForFirstIndex);
      
      year := inputYear;  -- Assigning the valid year with :=
      lang := "English";  -- Placeholder value with :=
   end readCalInfo;
   
begin
   -- Test readCalInfo
   readCalInfo(year, firstDay, lang);
   put_Line("Year: " & Integer'Image(year) & ", FirstDay: " & firstDay & ", Lang: " & lang);
end cal;
