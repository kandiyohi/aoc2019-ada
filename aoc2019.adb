with Ada.Text_IO;

with Ada.Text_IO.Unbounded_IO;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Ada.Directories;

with Day_1;

with Ada.Containers.Doubly_Linked_Lists;

with Get_Integer_Input;
use Get_Integer_Input;

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
	Input_File_Name: Unbounded_String := To_Unbounded_String("");
	Search: Directories.Search_Type;
	Directory_Entry: Directories.Directory_Entry_Type;
	File_Name_List: Unbounded_String_List.List := Unbounded_String_List.Empty_List;
	File_Name: Unbounded_String;
	Count: Integer;
	Response: Unbounded_String;
	Response_Integer: Integer;
	Input_Directory: Unbounded_String := To_Unbounded_String("inputs/");
	Day: Integer;
begin
	Day := Get_Integer("Specify which day to solve: ");
	case Day is
		when 1 =>
			Procedure_To_Run := Day_1.Day_1'Access;
		when others =>
			Text_IO.Put("ERROR: Day is not implemented.");
			return 1;
	end case;

	Directories.Start_Search(Search, To_String(Input_Directory), "*.txt");
	while Directories.More_Entries(Search) loop
		Directories.Get_Next_Entry(Search, Directory_Entry);
		Unbounded_String_List.Append(File_Name_List, To_Unbounded_String(Directories.Simple_Name(Directory_Entry)));
	end loop;
	Directories.End_Search(Search);

	Text_IO.Put_Line("Input files: ");
	Count := 0;
	for File_Name of File_Name_List loop
		Count := Count + 1;
		Text_IO.Put(Integer'Image(Count));
		Text_IO.Put(". ");
		Unbounded_IO.Put_Line(File_Name);
	end loop;
	Text_IO.Put("Select input file: ");
	Response := Unbounded_IO.Get_Line;
	-- TODO: Catch exception on this to retry input.
	Response_Integer := Integer'Value(To_String(Response));
	Count := 0;
	for File_Name of File_Name_List loop
		Count := Count + 1;
		if Count = Response_Integer then
			Input_File_Name := Input_Directory & File_Name;
			exit;
		end if;
	end loop;

	if Input_File_Name = To_Unbounded_String("") then
		Text_IO.Put_Line("No file name selected.");
		return 1;
	end if;
	Text_IO.Put_Line(To_String(Input_File_Name));
	--Text_IO.Put_Line("Which day to run? ");
	--Day_To_Run := Unbounded_IO.Get_Line;
	Procedure_To_Run.all(To_String(Input_File_Name));
	return 0;
end;
