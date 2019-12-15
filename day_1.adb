with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings;
use Ada.Strings;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;

with Fuel_Mass;
use Fuel_Mass;

-- --- Day 1: The Tyranny of the Rocket Equation ---
-- 
-- Santa has become stranded at the edge of the Solar System while delivering
-- presents to other planets! To accurately calculate his position in space,
-- safely align his warp drive, and return to Earth in time to save Christmas,
-- he needs you to bring him measurements from fifty stars.
-- 
-- Collect stars by solving puzzles. Two puzzles will be made available on each
-- day in the Advent calendar; the second puzzle is unlocked when you complete the
-- first. Each puzzle grants one star. Good luck!
-- 
-- The Elves quickly load you into a spacecraft and prepare to launch.
-- 
-- At the first Go / No Go poll, every Elf is Go until the Fuel Counter-Upper.
-- They haven't determined the amount of fuel required yet.
-- 
-- Fuel required to launch a given module is based on its mass. Specifically,
-- to find the fuel required for a module, take its mass, divide by three, round
-- down, and subtract 2.
-- 
-- For example:
-- 
--     For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2.
--     For a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel required is also 2.
--     For a mass of 1969, the fuel required is 654.
--     For a mass of 100756, the fuel required is 33583.
-- 
-- The Fuel Counter-Upper needs to know the total fuel requirement. To find it,
-- individually calculate the fuel needed for the mass of each module (your puzzle
-- input), then add together all the fuel values.
-- 
-- What is the sum of the fuel requirements for all of the modules on your
-- spacecraft?

package body Day_1 is

	procedure Day_1 is
		Input_File : File_Type;
		Mass : Integer;
		Mass_Needed : Integer := 0;
		Additional_Mass_Needed : Integer := 0;
		Intermediate_Mass : Integer := 0;
		Total_Mass_Needed : Integer := 0;
		package Int_IO is new Integer_IO(Integer); use Int_IO;
	begin
		Put_Line("CalculateFuelMass v2");
		Open(Input_File, In_File, "day_1.txt");
		while not End_Of_File(Input_File) loop
			Int_IO.Get(Input_File, Mass);
			Intermediate_Mass := Calculate_Fuel_Mass(Mass);
			Mass_Needed := Mass_Needed + Intermediate_Mass;
			Additional_Mass_Needed := Additional_Mass_Needed + Calculate_Fuel_Mass_Recursive(Intermediate_Mass);
		end loop;
		Put("Mass needed:");
		Put_Line(Integer'Image(Mass_Needed));
		Put("Additional mass needed:");
		Put_Line(Integer'Image(Additional_Mass_Needed));
		Total_Mass_Needed := Mass_Needed + Additional_Mass_Needed;
		Put("Total mass needed:");
		Put_Line(Integer'Image(Total_Mass_Needed));
	end;
end Day_1;
