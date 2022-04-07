use std::fs::read_to_string;

static FILENAME: &str = "input01";

fn calc_fuel(mass: isize) -> isize {
    mass / 3 - 2
}

fn calc_w_fuel(mass: isize) -> isize {
    let mut fuel = calc_fuel(mass);
    let mut total_fuel = 0;
    while fuel > 0 {
        total_fuel += fuel;
        fuel = calc_fuel(fuel);
    }

    total_fuel
}

fn main() -> std::io::Result<()> {
    let masses: Vec<_> = read_to_string(FILENAME)?
        .lines()
        .map(|l| l.parse::<isize>().unwrap())
        .collect();

    let fuel: isize = masses.into_iter().map(calc_w_fuel).sum();
    println!("{}", fuel);

    Ok(())
}
