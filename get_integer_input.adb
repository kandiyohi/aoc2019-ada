with Ada.Text_IO;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
use Ada.Text_IO.Unbounded_IO;

use Ada;
package body Get_Integer_Input is
	function Get_Integer(Prompt: String) return Integer is
		Response: Unbounded_String;
		Response_Integer: Integer;
	begin
		Text_IO.Put(Prompt);
		Response := Get_Line;
		Response_Integer := Integer'Value(To_String(Response));
		return Response_Integer;
	end Get_Integer;
end Get_Integer_Input;
