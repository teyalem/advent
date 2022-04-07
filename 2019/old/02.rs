use std::fs::read_to_string;

static FILENAME: &str = "input02";

fn get_addr(ops: &Vec<isize>, i: usize) -> usize {
    ops[i] as usize
}

fn eval(ops: &mut Vec<isize>, c: usize, rc: usize) -> bool {
    let ret = match ops[c] {
        op @ 1 | op @ 2 => {
            let o1 = get_addr(ops, c + 1);
            let o2 = get_addr(ops, c + 2);
            if op == 1 {
                ops[o1] + ops[o2]
            } else {
                ops[o1] * ops[o2]
            }
        }
        99 => {
            return false;
        }
        _ => {
            panic!("Unknown Operation");
        }
    };

    ops[rc] = ret;
    true
}

fn run(ops: &Vec<isize>, noun: isize, verb: isize) -> isize {
    let mut ops = ops.clone();
    ops[1] = noun;
    ops[2] = verb;

    let mut run = true;
    let mut pc = 0;
    while run {
        let rc = ops[pc + 3] as usize;
        run = eval(&mut ops, pc, rc);
        pc += 4;
    }

    ops[0]
}

fn main() -> std::io::Result<()> {
    let ops: Vec<_> = read_to_string(FILENAME)?
        .split(',')
        .map(|n| n.trim().parse::<isize>().unwrap())
        .collect();

    for noun in 0..100 {
        for verb in 0..100 {
            let out = run(&ops, noun, verb);
            if out == 19690720 {
                println!("{}", 100 * noun + verb);
                break;
            }
        }
    }

    Ok(())
}
