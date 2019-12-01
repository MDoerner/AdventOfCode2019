def main():
    mass_list = read_input("Advent20191201_1_input.txt")
    fuel_list = [full_requires_fuel(x) for x in mass_list]
    total_fuel = sum(fuel_list)
    print(total_fuel)

def read_input(filename):
    with open(filename, "r") as file:
        contents = [line for line in file]
    return [int(x) for x in contents]

def full_requires_fuel(mass):
    total_required_fuel = 0
    additionally_required_fuel = required_fuel(mass)
    while additionally_required_fuel > 0:
        total_required_fuel = total_required_fuel + additionally_required_fuel
        additionally_required_fuel = required_fuel(additionally_required_fuel)
    return total_required_fuel


def required_fuel(mass):
    base_fuel = (mass // 3) - 2
    return 0 if base_fuel < 0 else base_fuel

if __name__ == "__main__":
    main()