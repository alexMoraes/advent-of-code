package day15

import (
	"fmt"
	"io"
	"os"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

type Position struct {
	Row    int
	Column int
}

type Direction struct {
	DeltaRow    int
	DeltaColumn int
}

func parseInput(fileName string) (warehouse [][]rune, instructions string) {
	file, err := os.Open(fileName)
	check(err)
	defer file.Close()

	inputBytes, _ := io.ReadAll(file)
	input := string(inputBytes)
	input = strings.ReplaceAll(input, "\r\n", "\n")

	parts := strings.Split(input, "\n\n")

	warehouseMap := parts[0]
	instructions = strings.ReplaceAll(parts[1], "\n", "")
	for rowIndex, row := range strings.Split(warehouseMap, "\n") {
		warehouse = append(warehouse, []rune{})
		for _, position := range row {
			warehouse[rowIndex] = append(warehouse[rowIndex], position)
		}
	}

	return
}

func Part1() {
	warehouse, instructions := parseInput("./day15/inputs/input.txt")

	robot := getRobotPosition(warehouse)
	execute(instructions, robot, warehouse)

	sum := 0
	for rowIndex, row := range warehouse {
		for columnIndex, char := range row {
			if char == 'O' {
				sum += 100*rowIndex + columnIndex
			}
		}
	}

	fmt.Println(sum)
}

func Part2() {
	warehouse, instructions := parseInput("./day15/inputs/input.txt")

	for rowIndex, row := range warehouse {
		warehouse[rowIndex] = []rune{}
		for _, char := range row {
			var expandedFeatures []rune
			switch char {
			case '#':
				expandedFeatures = []rune{'#', '#'}
			case 'O':
				expandedFeatures = []rune{'[', ']'}
			case '.':
				expandedFeatures = []rune{'.', '.'}
			case '@':
				expandedFeatures = []rune{'@', '.'}
			}

			warehouse[rowIndex] = append(warehouse[rowIndex], expandedFeatures...)
		}
	}

	robot := getRobotPosition(warehouse)
	execute(instructions, robot, warehouse)

	sum := 0
	for rowIndex, row := range warehouse {
		for columnIndex, char := range row {
			if char == '[' {
				sum += 100*rowIndex + columnIndex
			}
		}
	}

	fmt.Println(sum)
}

func getDirection(instruction rune) (direction Direction) {
	switch instruction {
	case '^':
		direction = Direction{DeltaRow: -1, DeltaColumn: 0}
	case '>':
		direction = Direction{DeltaRow: 0, DeltaColumn: 1}
	case 'v':
		direction = Direction{DeltaRow: 1, DeltaColumn: 0}
	case '<':
		direction = Direction{DeltaRow: 0, DeltaColumn: -1}
	}

	return
}

func execute(instructions string, robot Position, warehouse [][]rune) {
	for _, instruction := range instructions {
		direction := getDirection(instruction)

		movementCandidates := []Position{robot}
		blocked := false
	movementLoop:
		for i := 0; i < len(movementCandidates); i++ {
			candidate := movementCandidates[i]
			nextPosition := Position{Row: candidate.Row + direction.DeltaRow, Column: candidate.Column + direction.DeltaColumn}
			switch warehouse[nextPosition.Row][nextPosition.Column] {
			case '.':
				continue
			case '#':
				blocked = true
				break movementLoop
			case 'O':
				movementCandidates = addCandidate(movementCandidates, nextPosition)
			case '[':
				partnerDirection := getDirection('>')
				movementCandidates = addCandidate(movementCandidates, nextPosition)
				movementCandidates = addCandidate(movementCandidates, Position{Row: nextPosition.Row + partnerDirection.DeltaRow, Column: nextPosition.Column + partnerDirection.DeltaColumn})
			case ']':
				partnerDirection := getDirection('<')
				movementCandidates = addCandidate(movementCandidates, nextPosition)
				movementCandidates = addCandidate(movementCandidates, Position{Row: nextPosition.Row + partnerDirection.DeltaRow, Column: nextPosition.Column + partnerDirection.DeltaColumn})
			}
		}

		if !blocked {
			robot = move(robot, movementCandidates, direction, warehouse)
		}
	}
}

func addCandidate(movementCandidates []Position, candidate Position) []Position {
	found := false
	for _, c := range movementCandidates {
		if c == candidate {
			found = true
			break
		}
	}

	if !found {
		movementCandidates = append(movementCandidates, candidate)
	}

	return movementCandidates
}

func move(robot Position, movementCandidates []Position, direction Direction, warehouse [][]rune) Position {
	for i := len(movementCandidates) - 1; i >= 0; i-- {
		candidate := movementCandidates[i]
		warehouse[candidate.Row+direction.DeltaRow][candidate.Column+direction.DeltaColumn] = warehouse[candidate.Row][candidate.Column]
		warehouse[candidate.Row][candidate.Column] = '.'
	}

	robot.Row += direction.DeltaRow
	robot.Column += direction.DeltaColumn

	return robot
}

func getRobotPosition(warehouse [][]rune) (robot Position) {
loop:
	for rowIndex, row := range warehouse {
		for columnIndex, char := range row {
			if char == '@' {
				robot.Row = rowIndex
				robot.Column = columnIndex
				break loop
			}
		}
	}

	return
}
