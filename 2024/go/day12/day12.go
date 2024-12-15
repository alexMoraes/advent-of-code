package day12

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

var up Step = Step{DeltaRow: -1, DeltaColumn: 0}
var left Step = Step{DeltaRow: 0, DeltaColumn: 1}
var right Step = Step{DeltaRow: 0, DeltaColumn: -1}
var down Step = Step{DeltaRow: 1, DeltaColumn: 0}
var directions []Step = []Step{up, left, down, right}

var visited map[Position]bool

func parseInput(fileName string) (garden [][]rune) {
	file, err := os.Open(fileName)
	check(err)
	defer file.Close()

	inputBytes, _ := io.ReadAll(file)
	input := string(inputBytes)
	input = strings.ReplaceAll(input, "\r\n", "\n")

	for rowIndex, row := range strings.Split(input, "\n") {
		garden = append(garden, []rune{})
		for _, plant := range row {
			garden[rowIndex] = append(garden[rowIndex], plant)
		}
	}

	return
}

func Part1() {
	garden := parseInput("./day12/inputs/input.txt")

	visited = make(map[Position]bool)

	price := 0
	for rowIndex, row := range garden {
		for columnIndex := range row {
			position := Position{Row: rowIndex, Column: columnIndex}
			isVisited := visited[position]
			if isVisited {
				continue
			}

			area, perimeter, _ := fill(position, garden)
			price += area * perimeter
		}
	}

	fmt.Println(price)
}

func Part2() {
	garden := parseInput("./day12/inputs/input.txt")

	visited = make(map[Position]bool)

	price := 0
	for rowIndex, row := range garden {
		for columnIndex := range row {
			position := Position{Row: rowIndex, Column: columnIndex}
			isVisited := visited[position]
			if isVisited {
				continue
			}

			area, _, sides := fill(position, garden)
			price += area * sides
		}
	}

	fmt.Println(price)
}

func fill(startingPosition Position, garden [][]rune) (area int, perimeter int, sides int) {
	rows := len(garden)
	columns := len(garden[0])

	includedPlots := make(map[Position]bool)
	plotPlant := garden[startingPosition.Row][startingPosition.Column]
	area = 0
	perimeter = 0
	candidates := []Position{startingPosition}
	for i := 0; i >= 0; i = len(candidates) - 1 {
		plot := candidates[i]
		candidates = candidates[0:i]

		included := includedPlots[plot]
		if included {
			continue
		}

		visited[plot] = true
		includedPlots[plot] = true
		area++

		corners := 0
		for directionIndex, direction := range directions {
			neighbor := Position{Row: plot.Row + direction.DeltaRow, Column: plot.Column + direction.DeltaColumn}

			nextDirection := directions[(directionIndex+1)%len(directions)]
			nextNeighbor := Position{Row: plot.Row + nextDirection.DeltaRow, Column: plot.Column + nextDirection.DeltaColumn}

			diagonalNeighbor := Position{Row: neighbor.Row + nextDirection.DeltaRow, Column: neighbor.Column + nextDirection.DeltaColumn}

			if neighbor.Row < 0 || neighbor.Row >= rows || neighbor.Column < 0 || neighbor.Column >= columns {
				perimeter++

				if nextNeighbor.Row < 0 || nextNeighbor.Row >= rows || nextNeighbor.Column < 0 || nextNeighbor.Column >= columns || garden[nextNeighbor.Row][nextNeighbor.Column] != plotPlant {
					corners++
				}

				continue
			}

			neighborIncluded := includedPlots[neighbor]
			if garden[neighbor.Row][neighbor.Column] == plotPlant && !neighborIncluded {
				candidates = append(candidates, neighbor)
			}

			if neighborIncluded {
				perimeter--
			} else {
				perimeter++
			}

			isNextNeighborWithinBounds := nextNeighbor.Row >= 0 && nextNeighbor.Row < rows && nextNeighbor.Column >= 0 && nextNeighbor.Column < columns

			if (garden[neighbor.Row][neighbor.Column] != plotPlant && (!isNextNeighborWithinBounds || garden[nextNeighbor.Row][nextNeighbor.Column] != plotPlant)) ||
				(garden[neighbor.Row][neighbor.Column] == plotPlant && isNextNeighborWithinBounds && garden[nextNeighbor.Row][nextNeighbor.Column] == plotPlant && garden[diagonalNeighbor.Row][diagonalNeighbor.Column] != plotPlant) {
				corners++
			}
		}

		sides += corners
	}

	return
}
