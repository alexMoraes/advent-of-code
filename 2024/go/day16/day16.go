package day16

import (
	"io"
	"os"
	"sort"
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

type Node struct {
	Position       Position
	DirectionIndex int
	EstimatedCost  int
	Cost           int
}

var north Direction = Direction{DeltaRow: -1, DeltaColumn: 0}
var east Direction = Direction{DeltaRow: 0, DeltaColumn: 1}
var south Direction = Direction{DeltaRow: 1, DeltaColumn: 0}
var west Direction = Direction{DeltaRow: 0, DeltaColumn: -1}
var directions []Direction = []Direction{east, south, west, north}

func parseInput(fileName string) (maze [][]rune, start Position, end Position) {
	file, err := os.Open(fileName)
	check(err)
	defer file.Close()

	inputBytes, _ := io.ReadAll(file)
	input := string(inputBytes)
	input = strings.ReplaceAll(input, "\r\n", "\n")

	maze = [][]rune{}
	for rowIndex, row := range strings.Split(input, "\n") {
		maze = append(maze, []rune{})
		for columnIndex, place := range row {
			maze[rowIndex] = append(maze[rowIndex], place)

			if place == 'S' {
				start.Row = rowIndex
				start.Column = columnIndex
			} else if place == 'E' {
				end.Row = rowIndex
				end.Column = columnIndex
			}

		}
	}

	return
}

func Part1() {
	maze, start, end := parseInput("./day16/inputs/input.txt")
	score := search(maze, start, end)

	println(score)
}

func Part2() {
}

func search(maze [][]rune, start Position, end Position) (cost int) {
	open := []Node{}
	closed := []Node{}

	turnDirections := []int{0, 1, 3}
	open = append(open, Node{Position: start, DirectionIndex: 0, EstimatedCost: manhattanDistance(start, end)})
	for len(open) > 0 {
		sort.Slice(open, func(i, j int) bool { return open[i].EstimatedCost < open[j].EstimatedCost })
		node := open[0]

		if node.Position == end {
			cost = node.Cost
			return
		}

		turningCost := 0
		stepCost := 1
		for _, turnDirection := range turnDirections {
			if turnDirection != 0 {
				turningCost = 1000
			}

			tentativeCost := node.Cost + turningCost + stepCost
			updatedEstimation := tentativeCost + manhattanDistance(node.Position, end)

			updatedDirectionIdex := (node.DirectionIndex + turnDirection) % len(directions)
			direction := directions[updatedDirectionIdex]
			neighborPosition := Position{Row: node.Position.Row + direction.DeltaRow, Column: node.Position.Column + direction.DeltaColumn}

			// If it's a wall, skip
			if maze[neighborPosition.Row][neighborPosition.Column] == '#' {
				continue
			}

			// If a better path was found, skip
			found, neighbor := find(neighborPosition, open)
			if found && updatedEstimation >= neighbor.EstimatedCost {
				continue
			}

			// If already explored, skip
			isClosed, _ := find(neighborPosition, closed)
			if isClosed {
				continue
			}

			neighbor.Position = neighborPosition
			neighbor.Cost = tentativeCost
			neighbor.EstimatedCost = updatedEstimation
			neighbor.DirectionIndex = updatedDirectionIdex
			open = append(open, neighbor)
		}

		open = open[1:]
		closed = append(closed, node)
	}

	return
}

func manhattanDistance(left, right Position) int {
	return abs(left.Row-right.Row) + abs(left.Column-right.Column)
}

func abs(value int) int {
	if value >= 0 {
		return value
	}

	return -value
}

func find(position Position, nodes []Node) (found bool, node Node) {
	for _, candidate := range nodes {
		if candidate.Position == position {
			return true, candidate
		}
	}
	return false, Node{}
}
