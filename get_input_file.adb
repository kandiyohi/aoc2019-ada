with Ada.Text_IO.Unbounded_IO;
use Ada.Text_IO.Unbounded_IO;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Strings;
use Ada.Strings;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Ada.Directories;
use Ada.Directories;

with Ada.Containers.Doubly_Linked_Lists;

with Get_Integer_Input;
use Get_Integer_Input;

with Get_Input_File;
use Get_Input_File;
package body Get_Input_File is
	function Get_Input_File(Prompt_1: String; Prompt_2: String; Input_Directory: String; Input_File_Pattern: String) return Unbounded_String is
		package Unbounded_String_List is new Ada.Containers.Doubly_Linked_Lists (Unbounded_String);
		Search: Search_Type;
		Directory_Entry: Directory_Entry_Type;
		File_Name_List: Unbounded_String_List.List := Unbounded_String_List.Empty_List;
		Count: Integer;
		Response: Unbounded_String;
		Response_Integer: Integer;
		Input_File_Name: Unbounded_String;
	begin
		Start_Search(Search, Input_Directory, Input_File_Pattern);
		while More_Entries(Search) loop
			Get_Next_Entry(Search, Directory_Entry);
			Unbounded_String_List.Append(File_Name_List, To_Unbounded_String(Simple_Name(Directory_Entry)));
		end loop;
		End_Search(Search);

		Put_Line(Prompt_1);
		Count := 0;
		for File_Name of File_Name_List loop
			Count := Count + 1;
			Put(Integer'Image(Count));
			Put(". ");
			Unbounded_IO.Put_Line(File_Name);
		end loop;
		Put(Prompt_2);
		Response := Unbounded_IO.Get_Line;
		-- TODO: Catch exception on this to retry input.
		Response_Integer := Integer'Value(To_String(Response));
		Count := 0;
		for File_Name of File_Name_List loop
			Count := Count + 1;
			if Count = Response_Integer then
				Input_File_Name := Input_Directory & File_Name;
				return Input_File_Name;
			end if;
		end loop;
		raise Program_Error with "No input file has corresponding value.";
	end Get_Input_File;
end Get_Input_File;
