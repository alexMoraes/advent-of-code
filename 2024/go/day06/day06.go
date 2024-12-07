package day06

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

func parseInput(fileName string) (guardStartingPosition Position, grid [][]rune) {
	file, err := os.Open(fileName)
	check(err)
	defer file.Close()

	inputBytes, _ := io.ReadAll(file)
	input := string(inputBytes)

	rows := strings.Split(input, "\n")
	for rowNumber, row := range rows {
		grid = append(grid, []rune{})
		for columnNumber, cell := range row {
			if cell == '^' {
				guardStartingPosition = Position{Row: rowNumber, Column: columnNumber}
			}
			grid[rowNumber] = append(grid[rowNumber], cell)
		}
	}

	return
}

func Part1() {
	guardStartingPosition, grid := parseInput("./day06/inputs/input.txt")

	visitedPositions, _ := walk(guardStartingPosition, grid)

	fmt.Println(len(visitedPositions))
}

func Part2() {
	guardStartingPosition, grid := parseInput("./day06/inputs/input.txt")

	// Only obstacles along the guard's path can influence it
	visitedPositions, _ := walk(guardStartingPosition, grid)

	loops := 0
	for position := range visitedPositions {
		if position == guardStartingPosition {
			continue
		}

		// Place obstacle
		grid[position.Row][position.Column] = '#'

		_, isLoop := walk(guardStartingPosition, grid)
		if isLoop {
			loops++
		}

		// Remove obstacle
		grid[position.Row][position.Column] = '.'
	}

	fmt.Println(loops)
}

func walk(position Position, grid [][]rune) (map[Position]bool, bool) {
	direction := '^'

	type LoopCheckerKey struct {
		Position  Position
		Direction rune
	}
	visitedPositions := map[Position]bool{}
	loopChecker := map[LoopCheckerKey]bool{}

loop:
	for {
		loopCheckerKey := LoopCheckerKey{Position: position, Direction: direction}
		_, visited := loopChecker[loopCheckerKey]
		if visited {
			return visitedPositions, true
		}

		visitedPositions[position] = true
		loopChecker[loopCheckerKey] = true
		candidatePosition, blocked, leftRoom := step(position, direction, grid)

		switch {
		case leftRoom:
			break loop
		case blocked:
			direction = turn(direction)
		default:
			position = candidatePosition
		}
	}

	return visitedPositions, false
}

func step(position Position, direction rune, grid [][]rune) (newPosition Position, blocked bool, leftRoom bool) {
	maxRow := len(grid) - 1
	maxColumn := len(grid[0]) - 1

	newPosition = position

	blocked = false
	leftRoom = false
	switch direction {
	case '^':
		newPosition.Row--
	case '>':
		newPosition.Column++
	case 'v':
		newPosition.Row++
	case '<':
		newPosition.Column--
	}

	leftRoom = newPosition.Row > maxRow || newPosition.Row < 0 || newPosition.Column > maxColumn || newPosition.Column < 0
	blocked = !leftRoom && grid[newPosition.Row][newPosition.Column] == '#'

	return
}

func turn(direction rune) (newDirection rune) {
	switch direction {
	case '^':
		return '>'
	case '>':
		return 'v'
	case 'v':
		return '<'
	case '<':
		return '^'
	default:
		return
	}
}
