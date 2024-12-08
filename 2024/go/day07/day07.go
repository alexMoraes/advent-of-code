package day07

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

type Equation struct {
	Result int
	Values []int
}

func parseInput(fileName string) (equations []Equation) {
	file, err := os.Open(fileName)
	check(err)
	defer file.Close()

	inputBytes, _ := io.ReadAll(file)
	input := string(inputBytes)
	input = strings.ReplaceAll(input, "\r\n", "\n")

	rows := strings.Split(input, "\n")
	for _, equationValues := range rows {
		equationStr := strings.Split(equationValues, ": ")
		valuesStr := strings.Split(equationStr[1], " ")

		result, _ := strconv.Atoi(equationStr[0])
		var values []int
		for _, v := range valuesStr {
			value, _ := strconv.Atoi(v)
			values = append(values, value)
		}

		equations = append(equations, Equation{Result: result, Values: values})
	}

	return
}

func Part1() {
	equations := parseInput("./day07/inputs/input.txt")
	possibleOperations := 2

	sum := operateAll(equations, possibleOperations)
	println(sum)
}

func Part2() {
	equations := parseInput("./day07/inputs/input.txt")
	possibleOperations := 3

	sum := operateAll(equations, possibleOperations)
	println(sum)
}

func nextOperationSequence(sequence []int, operations int) ([]int, bool) {
	overflow := true
	for i := range sequence {
		sequence[i]++
		sequence[i] %= operations

		if sequence[i] != 0 {
			overflow = false
			break
		}
	}

	return sequence, overflow
}

func operateAll(equations []Equation, possibleOperations int) int {
	sum := 0
	for _, equation := range equations {
		operations := len(equation.Values) - 1

		operationSequence := make([]int, operations) // An array of 0's and length 'operations'
		overflow := false
		for !overflow {
			result := performOperations(equation, operationSequence)
			if result == equation.Result {
				sum += result
				break
			}

			operationSequence, overflow = nextOperationSequence(operationSequence, possibleOperations)
		}
	}

	return sum
}

func performOperations(equation Equation, operationSequence []int) (result int) {
	result = equation.Values[0]

	for operationIndex, operation := range operationSequence {
		valueIndex := operationIndex + 1
		switch operation {
		case 0:
			result += equation.Values[valueIndex]
		case 1:
			result *= equation.Values[valueIndex]
		case 2:
			concatenated := strconv.Itoa(result) + strconv.Itoa(equation.Values[valueIndex])
			result, _ = strconv.Atoi(concatenated)
		}
	}

	return
}
