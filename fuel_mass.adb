package body Fuel_Mass is
	function Calculate_Fuel_Mass_Recursive(Mass: Integer) return Integer is
		Calculated_Mass: Integer := Calculate_Fuel_Mass(Mass);
	begin
		if Calculated_Mass = 0 then
			return Calculated_Mass;
		end if;
		return Calculated_Mass + Calculate_Fuel_Mass_Recursive(Calculated_Mass);
	end Calculate_Fuel_Mass_Recursive;

	function Calculate_Fuel_Mass(Mass: Integer) return Integer is
		Calculated_Mass: Integer := Mass / 3 - 2;
	begin
		if Calculated_Mass <= 0 then
			return 0;
		end if;
		return Calculated_Mass;
	end Calculate_Fuel_Mass;
end Fuel_Mass;
