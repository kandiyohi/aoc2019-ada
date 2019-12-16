with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Get_Integer_Input;
use Get_Integer_Input;

with Get_Input_File;
use Get_Input_File;

with Day_1;
with Day_2;

function aoc2019 return Integer is
	package Text_IO renames Ada.Text_IO;

	type Procedure_To_Run_Type is access procedure(Input_File_Path: String);

	Procedure_To_Run: Procedure_To_Run_Type;
	Input_File_Name: Unbounded_String;
	Day: Integer;
	Input_File_Pattern: Unbounded_String;
begin
	-- Get which day we should run.  We should eventually give a list of all
	-- days we can support.
	Day := Get_Integer("Specify which day to solve: ");
	case Day is
		when 1 =>
			Procedure_To_Run := Day_1.Day_1'Access;
			Input_File_Pattern := To_Unbounded_String("day_1*.txt");
		when 2 =>
			Procedure_To_Run := Day_2.Day_2'Access;
			Input_File_Pattern := To_Unbounded_String("day_2*.txt");
		when others =>
			Text_IO.Put("ERROR: Day is not implemented.");
			return 1;
	end case;

	-- Get which input file to use based on the day from inputs/.
	Input_File_Name := Get_Input_File.Get_Input_File("Input files: ", "Select input files: ", "inputs/", To_String(Input_File_Pattern));
	if Input_File_Name = To_Unbounded_String("") then
		Text_IO.Put_Line("ERROR: No file name selected.");
		Text_IO.Put_Line("Please retry program and select a valid option.");
		return 1;
	end if;

	-- Run the selected day procedure with the input file.
	Procedure_To_Run.all(To_String(Input_File_Name));

	return 0;
end;
