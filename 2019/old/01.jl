function calculate_fuel(mass)
	floor(mass / 3) - 2
end

function calc_w_fuel(mass)
	fuel = calculate_fuel(mass)

	f = fuel
	while (f = calculate_fuel(f)) >= 0
		fuel += f
	end

	fuel
end

total_fuel = open("input01") do f
	ns = [ parse(Int, n) for n in readlines(f) ]
	sum(map(calc_w_fuel, ns))
end

println(Int(total_fuel))
