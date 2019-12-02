def main():
    mass_list = read_input("Advent20191201_1_input.txt")
    fuel_list = [required_fuel(x) for x in mass_list]
    total_fuel = sum(fuel_list)
    print(total_fuel)

def read_input(filename):
    with open(filename, "r") as file:
        contents = [line for line in file]
    return [int(x) for x in contents]

def required_fuel(mass):
    return (mass // 3) - 2

if __name__ == "__main__":
    main()