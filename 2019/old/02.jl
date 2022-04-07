mops = open("input02") do f
	data = read(f, String)
	[ parse(Int, n) for n in split(data, ',')]
end

function eval(ops, c, rc) #operations, counter, return counter
	let ret =
		if ops[c] == 1 #plus
			ops[ops[c+1] + 1] + ops[ops[c+2] + 1]
		elseif ops[c] == 2 #multiply
			ops[ops[c+1] + 1] * ops[ops[c+2] + 1]
		elseif ops[c] == 99 #end
			return false
		else
			error("Unknown Opcode")
		end

		ops[rc] = ret
		true
	end
end

function run(ops)
	for i in 0:4:length(ops)
		run = eval(ops, i + 1, ops[i+4] + 1)
		if !run
			break
		end
	end
end

for noun in 0:99
	for verb in 0:99

		#new ops and put args
		ops = copy(mops)
		ops[2] = noun
		ops[3] = verb
		run(ops)

		if ops[1] == 19690720
			print(100 * noun + verb)
			break
		end
	end
end
