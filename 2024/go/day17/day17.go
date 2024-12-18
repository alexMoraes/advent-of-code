package day17

import (
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

type Computer struct {
	ProgramCounter int
	RegisterA      int
	RegisterB      int
	RegisterC      int
	Program        []int
}

func parseInput(fileName string) (computer Computer) {
	file, err := os.Open(fileName)
	check(err)
	defer file.Close()

	inputBytes, _ := io.ReadAll(file)
	input := string(inputBytes)
	input = strings.ReplaceAll(input, "\r\n", "\n")

	parts := strings.Split(input, "\n\n")

	registersPart := parts[0]
	registers := strings.Split(registersPart, "\n")
	computer.RegisterA = parseRegister(registers[0])
	computer.RegisterB = parseRegister(registers[1])
	computer.RegisterC = parseRegister(registers[2])

	instructions := strings.ReplaceAll(parts[1], "Program: ", "")
	for _, instruction := range strings.Split(instructions, ",") {
		opcode, _ := strconv.Atoi(instruction)
		computer.Program = append(computer.Program, opcode)
	}

	return
}

func Part1() {
	computer := parseInput("./day17/inputs/input.txt")

	output := []string{}
	for _, value := range compute(computer) {
		output = append(output, strconv.Itoa(value))
	}

	println(strings.Join(output, ","))
}

func Part2() {
	computer := parseInput("./day17/inputs/input.txt")

	registerA, _ := reverseComputation(0, computer.Program, len(computer.Program)-1)

	println(registerA)
}

func parseRegister(register string) int {
	parts := strings.Split(register, " ")
	value, _ := strconv.Atoi(parts[2])
	return value
}

func compute(computer Computer) []int {
	output := []int{}
	for computer.ProgramCounter = 0; computer.ProgramCounter < len(computer.Program); computer.ProgramCounter += 2 {
		opcode := computer.Program[computer.ProgramCounter]
		operand := computer.Program[computer.ProgramCounter+1]

		switch opcode {
		case 0:
			// adv
			operand = decodeComboOperand(operand, computer)
			computer.RegisterA = computer.RegisterA >> operand // x / pow(2, operand) = x >> operand

		case 1:
			// bxl
			computer.RegisterB = computer.RegisterB ^ operand

		case 2:
			// bst
			operand = decodeComboOperand(operand, computer)
			computer.RegisterB = operand % 8

		case 3:
			// jnz
			if computer.RegisterA != 0 {
				computer.ProgramCounter = operand - 2 // Subtracts 2 so the loop variable sets it back to te correct value
			}

		case 4:
			// bxc
			computer.RegisterB = computer.RegisterB ^ computer.RegisterC

		case 5:
			// out
			operand = decodeComboOperand(operand, computer)
			output = append(output, operand%8)

		case 6:
			// bdv
			operand = decodeComboOperand(operand, computer)
			computer.RegisterB = computer.RegisterA >> operand // x / pow(2, operand) = x >> operand

		case 7:
			// cdv
			operand = decodeComboOperand(operand, computer)
			computer.RegisterC = computer.RegisterA >> operand // x / pow(2, operand) = x >> operand
		}
	}

	return output
}

func decodeComboOperand(comboOperand int, computer Computer) (operand int) {
	switch {
	case comboOperand <= 3:
		operand = comboOperand
	case comboOperand == 4:
		operand = computer.RegisterA
	case comboOperand == 5:
		operand = computer.RegisterB
	case comboOperand == 6:
		operand = computer.RegisterC
	}

	return
}

func reverseComputation(registerA int, program []int, index int) (updatedRegister int, found bool) {
	for increment := 0; increment < 8; increment++ {
		updatedRegister = (registerA << 3) | increment

		output := compute(Computer{RegisterA: updatedRegister, Program: program})
		if output[0] == program[index] {
			if index == 0 {
				found = true
				return
			} else {
				updatedRegister, found = reverseComputation(updatedRegister, program, index-1)
				if found {
					return
				}
			}
		}
	}

	return
}
