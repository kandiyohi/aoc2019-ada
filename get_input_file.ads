with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package Get_Input_File is
	function Get_Input_File(Prompt_1: String; Prompt_2: String; Input_Directory: String; Input_File_Pattern: String) return Unbounded_String;
end Get_Input_File;
