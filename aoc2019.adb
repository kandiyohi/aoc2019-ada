with Ada.Text_IO;

with Ada.Text_IO.Unbounded_IO;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Ada.Directories;

with Day_1;

with Ada.Containers.Doubly_Linked_Lists;

with Get_Integer_Input;
use Get_Integer_Input;

with Get_Input_File;
use Get_Input_File;

function aoc2019 return Integer is
	-- It's important to distinguish between Text_IO and Unbounded_IO.  For
	-- printing, Text_IO is easier to user, but for input, Unbounded_IO is far
	-- easier to use.
	package Text_IO renames Ada.Text_IO;
	package Unbounded_IO renames Ada.Text_IO.Unbounded_IO;
	package Directories renames Ada.Directories;
	package Unbounded_String_List is new Ada.Containers.Doubly_Linked_Lists (Unbounded_String);
	package Integer_IO is new Text_IO.Integer_IO (Integer);

	type Procedure_To_Run_Type is access procedure(Input_File_Path: String);

	Procedure_To_Run: Procedure_To_Run_Type;
	Day_To_Run: Unbounded_String;
	Input_File_Name: Unbounded_String;
	File_Name: Unbounded_String;
	Response: Unbounded_String;
	Input_Directory: Unbounded_String := To_Unbounded_String("inputs/");
	Day: Integer;
	Input_File_Pattern: Unbounded_String;
begin
	Day := Get_Integer("Specify which day to solve: ");
	case Day is
		when 1 =>
			Procedure_To_Run := Day_1.Day_1'Access;
			Input_File_Pattern := To_Unbounded_String("day_1*.txt");
		when others =>
			Text_IO.Put("ERROR: Day is not implemented.");
			return 1;
	end case;

	Input_File_Name := Get_Input_File.Get_Input_File("Input files: ", "Select input files: ", "inputs/", "day_1*.txt");

	if Input_File_Name = To_Unbounded_String("") then
		Text_IO.Put_Line("ERROR: No file name selected.");
		Text_IO.Put_Line("Please retry program and select a valid option.");
		return 1;
	end if;
	Procedure_To_Run.all(To_String(Input_File_Name));
	return 0;
end;
