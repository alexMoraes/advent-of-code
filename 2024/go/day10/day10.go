package day10

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

type Step struct {
	DeltaRow    int
	DeltaColumn int
}

func parseInput(fileName string) (trailheads []Position, elevationMap [][]int) {
	file, err := os.Open(fileName)
	check(err)
	defer file.Close()

	inputBytes, _ := io.ReadAll(file)
	input := string(inputBytes)
	input = strings.ReplaceAll(input, "\r\n", "\n")

	for rowIndex, row := range strings.Split(input, "\n") {
		elevationMap = append(elevationMap, []int{})

		for columnIndex, elevation := range row {
			elevationMap[rowIndex] = append(elevationMap[rowIndex], int(elevation-'0'))
			if elevation == '0' {
				trailheads = append(trailheads, Position{Row: rowIndex, Column: columnIndex})
			}
		}
	}

	return
}

func Part1() {
	trailheads, elevationMap := parseInput("./day10/inputs/input.txt")

	totalScore := 0
	for _, trailhead := range trailheads {
		peaksReached := make(map[Position]bool)
		trace(trailhead, elevationMap, peaksReached)

		totalScore += len(peaksReached)
	}

	fmt.Println(totalScore)
}

func Part2() {
	trailheads, elevationMap := parseInput("./day10/inputs/input.txt")

	totalRating := 0
	for _, trailhead := range trailheads {
		peaksReached := make(map[Position]bool)
		totalRating += trace(trailhead, elevationMap, peaksReached)
	}

	fmt.Println(totalRating)
}

func trace(position Position, elevationMap [][]int, peaksReached map[Position]bool) (paths int) {
	if elevationMap[position.Row][position.Column] == 9 {
		peaksReached[position] = true
		paths = 1
		return
	}

	up := Step{DeltaRow: -1, DeltaColumn: 0}
	left := Step{DeltaRow: 0, DeltaColumn: 1}
	right := Step{DeltaRow: 0, DeltaColumn: -1}
	down := Step{DeltaRow: 1, DeltaColumn: 0}

	rows := len(elevationMap)
	columns := len(elevationMap[0])
	directions := []Step{up, left, down, right}

	paths = 0
	for _, direction := range directions {
		newPosition := Position{
			Row:    position.Row + direction.DeltaRow,
			Column: position.Column + direction.DeltaColumn,
		}

		if newPosition.Row < 0 || newPosition.Row >= rows || newPosition.Column < 0 || newPosition.Column >= columns {
			// Out of bounds, skip this direction
			continue
		}

		if elevationMap[newPosition.Row][newPosition.Column] == elevationMap[position.Row][position.Column]+1 {
			paths += trace(newPosition, elevationMap, peaksReached)
		}
	}

	return
}
