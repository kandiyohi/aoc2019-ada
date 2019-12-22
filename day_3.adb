with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Strings;
use Ada.Strings;

with Ada.Strings.Fixed;
use Ada.Strings.Fixed;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Ada.Containers;
use Ada.Containers;

with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;

with Ada.Characters.Latin_1;
use Ada.Characters.Latin_1;


package body Day_3 is
    type Wire_Coordinates_Type is
    record
        X: Integer;
        Y: Integer;
    end record;

    function Hash(Wire_Coordinates: Wire_Coordinates_Type) return Hash_Type is
    begin
        return Hash_Type'Mod(Wire_Coordinates.X + Wire_Coordinates.Y);
    end Hash;

    function Equivalent_Elements(Left, Right: Wire_Coordinates_Type) return Boolean is
    begin
        return Left.X = Right.X and Left.Y = Right.Y;
    end;

    package Wire_Coordinates_Set is new Ada.Containers.Hashed_Sets (Wire_Coordinates_Type, Hash, Equivalent_Elements);
    package Wire_Coordinates_Set_Vector is new Ada.Containers.Vectors (Natural, Wire_Coordinates_Set.Set, Wire_Coordinates_Set."=");
    package Integer_Vector is new Ada.Containers.Vectors (Natural, Natural);
    package Integer_Vector_Sorting is new Integer_Vector.Generic_Sorting;

    package Int_IO is new Integer_IO (Integer);


    type Direction_Type is (Up, Down, Left, Right);

    type Magnitude_Element_Type is tagged
        record
            Direction: Direction_Type;
            Magnitude: Integer;
        end record;

    procedure Parse_Direction(Input_File: in out File_Type; Direction: out Direction_Type) is
        Char: Character;
    begin
        Get(Input_File, Char);

        case Char is
            when 'U' =>
                Direction := Up;
            when 'D' =>
                Direction := Down;
            when 'L' =>
                Direction := Left;
            When 'R' =>
                Direction := Right;
            when others =>
                raise Program_Error with "Unknown direction character: " & Char;
        end case;
    end Parse_Direction;

    procedure Parse_Magnitude(Input_File: in out File_Type; Magnitude: out Integer) is
    begin
        Int_IO.Get(Input_File, Magnitude);
    end Parse_Magnitude;

    procedure Parse_Separator(Input_File: in out File_Type) is
        Char: Character;
    begin
        Get(Input_File, Char);
        if Char /= ',' then
            raise Program_Error with "Separator ',' expected, but got " & Char;
        end if;
    end Parse_Separator;

    procedure Parse_Magnitude_Element(Input_File: in out File_Type; Magnitude_Element: out Magnitude_Element_Type) is
    begin
        Parse_Direction(Input_File, Magnitude_Element.Direction);
        Parse_Magnitude(Input_File, Magnitude_Element.Magnitude);
    end Parse_Magnitude_Element;

    procedure Run_Wire(Magnitude_Element: Magnitude_Element_Type; Current_Coordinates: in out Wire_Coordinates_Type; Coordinates_Set: in out Wire_Coordinates_Set.Set) is
    begin
        for Coordinate_Difference in Integer range 1 .. Magnitude_Element.Magnitude loop
            case Magnitude_Element.Direction is
                -- Positives
                when Up =>
                    Current_Coordinates.Y := Current_Coordinates.Y + 1;
                when Right =>
                    Current_Coordinates.X := Current_Coordinates.X + 1;
                -- Negatives
                when Down =>
                    Current_Coordinates.Y := Current_Coordinates.Y - 1;
                when Left =>
                    Current_Coordinates.X := Current_Coordinates.X - 1;
            end case;
            if not Coordinates_Set.Contains(Current_Coordinates) then
                Coordinates_Set.Insert(Current_Coordinates);
            end if;
        end loop;
    end Run_Wire;

    procedure Parse_Magnitude_Set(Input_File: in out File_Type; Output_Set: out Wire_Coordinates_Set.Set) is
        Magnitude_Element: Magnitude_Element_Type;
        Current_Coordinates: Wire_Coordinates_Type;
        First_Element: Boolean := True;
    begin
        Current_Coordinates.X := 0;
        Current_Coordinates.Y := 0;

        Output_Set := Wire_Coordinates_Set.Empty_Set;

        while not End_Of_Line(Input_File) loop
            if not First_Element then
                Parse_Separator(Input_File);
            end if;
            Parse_Magnitude_Element(Input_File, Magnitude_Element);
            Magnitude_Element.Run_Wire(Current_Coordinates, Output_Set);
            First_Element := False;
        end loop;
    end Parse_Magnitude_Set;

    procedure Parse_Magnitude_Sets(Input_File: in out File_Type; Output_Sets: out Wire_Coordinates_Set_Vector.Vector) is
        Output_Set: Wire_Coordinates_Set.Set;
    begin
        while not End_Of_File(Input_File) loop
            Parse_Magnitude_Set(Input_File, Output_Set);
            if not End_Of_Line(Input_File) then
                raise Program_Error with "Expected end of line on line " & Ada.Text_IO.Count'Image(Line(Input_File)) & " column " & Ada.Text_IO.Count'Image(Col(Input_File));
            end if;
            Skip_Line(Input_File);
            Output_Sets.Append(Output_Set);
        end loop;
    end Parse_Magnitude_Sets;

    procedure Intersect_Sets(Sets_To_Intersect: Wire_Coordinates_Set_Vector.Vector; Intersect_Result: out Wire_Coordinates_Set.Set) is
        On_First_Set: Boolean := True;
    begin
        Intersect_Result := Sets_To_Intersect(0);
        for Set of Sets_To_Intersect loop
            if On_First_Set then
                On_First_Set := False;
                goto Continue;
            end if;

            Intersect_Result := Intersect_Result.Intersection(Set);

            <<Continue>>
        end loop;
    end Intersect_Sets;

    procedure Compute_Manhattan_Distances(Intersected_Set: Wire_Coordinates_Set.Set; Distances: out Integer_Vector.Vector) is
        Distance: Natural;
    begin
        Distances := Integer_Vector.Empty_Vector;
        for Intersection of Intersected_Set loop
            Distance := 0;
            if Intersection.X < 0 then
                Distance := -Intersection.X;
            else
                Distance := Intersection.X;
            end if;

            if Intersection.Y < 0 then
                Distance := Distance - Intersection.Y;
            else
                Distance := Distance + Intersection.Y;
            end if;
            Distances.Append(Distance);
        end loop;
    end Compute_Manhattan_Distances;

    procedure Day_3(Input_File_Name: String) is
        Input_file: File_Type;
        Output_Sets: Wire_Coordinates_Set_Vector.Vector;
        Intersections: Wire_Coordinates_Set.Set;
        Distances: Integer_Vector.Vector;
    begin
        Open(Input_File, In_File, Input_File_Name);
        Parse_Magnitude_Sets(Input_File, Output_Sets);
        if Output_Sets.Length = 0 then
            raise Program_error with "No sets from input file " & Input_File_Name;
        elsif Output_Sets.Length = 1 then
            Put_Line("0");
        end if;
        Intersect_Sets(Output_Sets, Intersections);
        Compute_Manhattan_Distances(Intersections, Distances);
        Integer_Vector_Sorting.Sort(Distances);
        Put("Minimum Manhattan distance to port: ");
        if Distances.Length = 0 then
            Put("inf");
        else
            Int_IO.Put(Distances(0));
        end if;
        New_Line;
        Close(Input_File);
    end Day_3;
end Day_3;
