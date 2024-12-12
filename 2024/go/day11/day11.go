package day11

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

func parseInput(fileName string) (stones []int) {
	file, err := os.Open(fileName)
	check(err)
	defer file.Close()

	inputBytes, _ := io.ReadAll(file)
	input := string(inputBytes)
	input = strings.Trim(input, "\r\n")

	for _, v := range strings.Split(input, " ") {
		value, _ := strconv.Atoi(v)
		stones = append(stones, value)
	}

	return
}

func Part1() {
	stones := parseInput("./day11/inputs/input.txt")

	stones = dumbSolution(stones, 25)

	fmt.Println(len(stones))
}

type KnownValueKey struct {
	Stone     int
	Remaining int
}

var knownValues map[KnownValueKey]int = make(map[KnownValueKey]int)

func Part2() {
	stones := parseInput("./day11/inputs/input.txt")

	total := 0
	for _, stone := range stones {
		total += smartSolution(stone, 75)
	}

	fmt.Println(total)
}

func smartSolution(stone int, steps int) int {
	key := KnownValueKey{Stone: stone, Remaining: steps}
	result, found := knownValues[key]

	if found {
		return result
	}

	if steps == 0 {
		return 1
	}

	if stone == 0 {
		result = smartSolution(1, steps-1)
	} else {
		stoneLabel := strconv.Itoa(stone)
		l := len(stoneLabel)

		if l%2 == 0 {
			left := stoneLabel[:l/2]
			right := stoneLabel[l/2:]

			leftValue, _ := strconv.Atoi(left)
			rightValue, _ := strconv.Atoi(right)

			result = smartSolution(leftValue, steps-1) + smartSolution(rightValue, steps-1)
		} else {
			result = smartSolution(stone*2024, steps-1)
		}
	}

	knownValues[key] = result
	return result
}

func dumbSolution(stones []int, steps int) []int {
	var newStones []int // Allocate a new array for the updated state

	// Iterate over the old state
	for i := 0; i < steps; i++ {
		newStones = []int{}

		for _, stone := range stones {
			if stone == 0 {
				newStones = append(newStones, 1)
			} else {
				stoneLabel := strconv.Itoa(stone)
				l := len(stoneLabel)

				if l%2 == 0 {
					left := stoneLabel[:l/2]
					right := stoneLabel[l/2:]

					leftValue, _ := strconv.Atoi(left)
					rightValue, _ := strconv.Atoi(right)

					newStones = append(newStones, leftValue)
					newStones = append(newStones, rightValue)
				} else {
					newStones = append(newStones, stone*2024)
				}
			}
		}

		stones = newStones
	}

	// Replace old state with new state
	return stones
}
