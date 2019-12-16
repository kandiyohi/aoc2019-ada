with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Characters.Handling;
use Ada.Characters.Handling;

with Ada.Containers.Vectors;

package body Day_2 is
	package Int_IO is new Integer_IO(Integer);

	package Integer_Vector is new Ada.Containers.Vectors(Natural, Integer);
	use Integer_Vector;

	function Read_Opcodes(Input_File_Name: String) return Vector is
		Input_File: File_Type;
		Opcode: Integer;
		Opcodes: Vector := Empty_Vector;
		Char: Character;
		End_Of_Line: Boolean;
	begin
		Open(Input_File, In_File, Input_File_Name);
		while not End_Of_File(Input_file) loop
			Look_Ahead(Input_File, Char, End_Of_Line);
			if End_Of_Line then
				-- Do nothing but skip.
				Get(Input_File, Char);
			elsif Char = ',' then
				-- Do nothing but skip.
				Get(Input_File, Char);
			elsif Is_Digit(Char) then
				Int_IO.Get(Input_File, Opcode);
				Append(Opcodes, Opcode);
			else
				raise Program_Error with "Unexpected character in input: " & Char;
			end if;
		end loop;
		Close(Input_File);
		return Opcodes;
	end Read_Opcodes;

	procedure Add(Opcodes: in out Vector; Input_Index_1, Input_Index_2, Output_Index: Integer) is
		Input_Value_1: Integer;
		Input_Value_2: Integer;
		Output_Value: Integer;
		Lowest_Index: Integer := Integer'Min(Input_Index_1, Input_Index_2);
		Highest_Index: Integer := Integer'Max(Input_Index_1, Input_Index_2);
	begin
		if Lowest_Index < First_Index(Opcodes) or Highest_Index > Last_Index(Opcodes) then
			return;
		end if;
		Input_Value_1 := Opcodes(Input_Index_1);
		Input_Value_2 := Opcodes(Input_Index_2);
		Output_Value := Input_Value_1 + Input_Value_2;
		Replace_Element(Opcodes, Output_Index, Output_Value);
	end Add;

	procedure Multiply(Opcodes: in out Vector; Input_Index_1, Input_Index_2, Output_Index: Integer) is
		Input_Value_1: Integer;
		Input_Value_2: Integer;
		Output_Value: Integer;
		Lowest_Index: Integer := Integer'Min(Input_Index_1, Input_Index_2);
		Highest_Index: Integer := Integer'Max(Input_Index_1, Input_Index_2);
	begin
		if Lowest_Index < First_Index(Opcodes) or Highest_Index > Last_Index(Opcodes) then
			return;
		end if;
		Input_Value_1 := Opcodes(Input_Index_1);
		Input_Value_2 := Opcodes(Input_Index_2);
		Output_Value := Input_Value_1 * Input_Value_2;
		Replace_Element(Opcodes, Output_Index, Output_Value);
	end Multiply;

	function Parse_Opcodes(Opcodes: Vector) return Vector is
		Modified_Opcodes: Vector := Copy(Opcodes);
		Opcode, Input_Index_1, Input_Index_2, Output_Index: Integer;
	begin
		for Index in First_Index(Modified_Opcodes)..Last_Index(Modified_Opcodes) loop
			if Index mod 4 /= 0 then
				goto Continue;
			end if;
			Opcode := Modified_Opcodes(Index);
			Input_Index_1 := Modified_Opcodes(Index+1);
			Input_Index_2 := Modified_Opcodes(Index+2);
			Output_Index := Modified_Opcodes(Index+3);
			case Opcode is
				when 1 =>
					Add(Modified_Opcodes, Input_Index_1, Input_Index_2, Output_Index);
				when 2 =>
					Multiply(Modified_Opcodes, Input_Index_1, Input_Index_2, Output_Index);
				when 99 =>
					return Modified_Opcodes;
				when others =>
					raise Program_Error with "Unknown opcode: " & Integer'Image(Opcode);
			end case;
			<<Continue>>
		end loop;
		raise Program_Error with "No halt statement.";
	end Parse_Opcodes;

	procedure Reset_State(Opcodes: in out Vector; Input_1, Input_2: Integer) is
	begin
		Opcodes(1) := Input_1;
		Opcodes(2) := Input_2;
	end Reset_State;

	procedure Put_Opcodes(Opcodes: Vector) is
		Count: Integer;
	begin
		Count := 0;
		Int_IO.Put(Count);
		Put("  --");
		for Opcode of Opcodes loop
			Count := Count + 1;
			Int_IO.Put(Standard_Output, Opcode);
			if Count mod 4 = 0 then
				New_Line;
				Int_IO.Put(Count);
				Put("  --");
			end if;
		end loop;
		New_Line;
		New_Line;
	end;

	procedure Day_2(Input_file_Name: String) is
		Opcodes: Vector := Read_Opcodes(Input_File_Name);
		Modified_Opcodes: Vector;
		Program_Output: Integer;
		Input: Integer;
	begin
		-- Reset the 12 02 alarm state.
		Reset_State(Opcodes, 12, 2);
		--Put_Opcodes(Opcodes);
		Modified_Opcodes := Parse_Opcodes(Opcodes);
		--Put_Opcodes(Modified_Opcodes);
		Program_Output := Modified_Opcodes(0);
		Put_Line("First position output: " & Integer'Image(Program_Output));

		for Input_1 in Integer range 1..255 loop
			for Input_2 in Integer range 1..255 loop
				null;
				Reset_State(Opcodes, Input_1, Input_2);
				Modified_Opcodes := Parse_Opcodes(Opcodes);
				Program_Output := Modified_Opcodes(0);
				if Program_Output = 19690720 then
					Input := Modified_Opcodes(1)*100 + Modified_Opcodes(2);
					Put_Line("Output 19690720 is given by input " & Integer'Image(Input));
					exit;
				end if;
			end loop;
		end loop;
	end Day_2;
end Day_2;
