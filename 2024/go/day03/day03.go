package day03

import (
	"fmt"
	"io"
	"math"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

type Instruction struct {
	Left  int
	Right int
}

func parseInput(fileName string) []Instruction {
	file, err := os.Open(fileName)
	check(err)
	defer file.Close()

	inputBytes, _ := io.ReadAll(file)
	input := string(inputBytes)

	validInstructions := regexp.MustCompile(`mul\((\d){1,3},(\d){1,3}\)|do\(\)|don't\(\)`)
	instructionsRaw := validInstructions.FindAllString(string(input), math.MaxInt32)

	var instructions []Instruction
	do := true
	for _, instructionRaw := range instructionsRaw {
		switch {
		case instructionRaw == "do()":
			do = true
		case instructionRaw == "don't()":
			do = false
		case do:
			trimmed := strings.TrimPrefix(instructionRaw, "mul(")
			trimmed = strings.TrimRight(trimmed, ")")

			parts := strings.Split(trimmed, ",")
			left, _ := strconv.Atoi(parts[0])
			right, _ := strconv.Atoi(parts[1])

			instructions = append(instructions, Instruction{Left: left, Right: right})
		}
	}

	return instructions
}

func Solution() {
	instructions := parseInput("./day03/inputs/input.txt")

	sum := 0
	for _, instruction := range instructions {
		sum += instruction.Left * instruction.Right
	}

	fmt.Println(sum)
}
