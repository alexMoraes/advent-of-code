package day13

import (
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

type Vector struct {
	X int
	Y int
}

type Machine struct {
	ButtonA Vector
	ButtonB Vector
	Prize   Vector
}

func parseInput(fileName string) (machines []Machine) {
	file, err := os.Open(fileName)
	check(err)
	defer file.Close()

	inputBytes, _ := io.ReadAll(file)
	input := string(inputBytes)
	input = strings.ReplaceAll(input, "\r\n", "\n")

	for _, machine := range strings.Split(input, "\n\n") {
		instructions := strings.Split(machine, "\n")
		buttonA := parseButton(instructions[0], "Button A")
		buttonB := parseButton(instructions[1], "Button B")
		prize := parsePrize(instructions[2])

		machines = append(machines, Machine{
			ButtonA: buttonA,
			ButtonB: buttonB,
			Prize:   prize,
		})
	}

	return
}

func parseButton(instruction string, buttonName string) (button Vector) {
	instruction = strings.ReplaceAll(instruction, buttonName, "")
	instruction = strings.ReplaceAll(instruction, ": X+", "")
	instruction = strings.ReplaceAll(instruction, "Y+", "")

	button = parseVector(instruction)
	return
}

func parsePrize(instruction string) (prize Vector) {
	instruction = strings.ReplaceAll(instruction, "Prize: X=", "")
	instruction = strings.ReplaceAll(instruction, "Y=", "")

	prize = parseVector(instruction)
	return
}

func parseVector(instruction string) (vector Vector) {
	values := strings.Split(instruction, ", ")
	xValue, _ := strconv.Atoi(values[0])
	yValue, _ := strconv.Atoi(values[1])

	vector = Vector{X: xValue, Y: yValue}
	return
}

func Part1() {
	machines := parseInput("./day13/inputs/input.txt")

	tokens := 0
	for _, machine := range machines {
		tokens += calculateTokens(machine)
	}

	fmt.Println(tokens)
}

func Part2() {
	machines := parseInput("./day13/inputs/input.txt")

	tokens := 0
	for _, machine := range machines {
		machine.Prize.X += 10000000000000
		machine.Prize.Y += 10000000000000

		tokens += calculateTokens(machine)
	}

	fmt.Println(tokens)
}

func calculateTokens(machine Machine) (tokens int) {
	tokens = 0
	det := machine.ButtonA.X*machine.ButtonB.Y - machine.ButtonA.Y*machine.ButtonB.X
	if det == 0 {
		return
	}

	bPresses := (machine.ButtonA.X*machine.Prize.Y - machine.ButtonA.Y*machine.Prize.X) / det
	aPresses := (machine.Prize.X - bPresses*machine.ButtonB.X) / machine.ButtonA.X

	// If not divisible, we would need a fraction of a push
	if (machine.ButtonA.X*machine.Prize.Y-machine.ButtonA.Y*machine.Prize.X)%det != 0 ||
		(machine.Prize.X-bPresses*machine.ButtonB.X)%machine.ButtonA.X != 0 {
		return
	}

	if bPresses >= 0 && aPresses >= 0 {
		tokens = bPresses + 3*aPresses
	}

	return
}
