package day18

import (
	"io"
	"os"
	"sort"
	"strconv"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

type Position struct {
	x int
	y int
}

type Direction struct {
	deltaX int
	deltaY int
}

type Node struct {
	position      Position
	estimatedCost int
	cost          int
	from          *Node
}

var north Direction = Direction{deltaY: -1, deltaX: 0}
var east Direction = Direction{deltaY: 0, deltaX: 1}
var south Direction = Direction{deltaY: 1, deltaX: 0}
var west Direction = Direction{deltaY: 0, deltaX: -1}
var directions []Direction = []Direction{east, south, west, north}

func parseInput(fileName string) (bytePositions []Position) {
	file, err := os.Open(fileName)
	check(err)
	defer file.Close()

	inputBytes, _ := io.ReadAll(file)
	input := string(inputBytes)
	input = strings.ReplaceAll(input, "\r\n", "\n")

	for _, line := range strings.Split(input, "\n") {
		coordinates := strings.Split(line, ",")
		x, _ := strconv.Atoi(coordinates[0])
		y, _ := strconv.Atoi(coordinates[1])

		bytePositions = append(bytePositions, Position{x: x, y: y})
	}

	return
}

func Part1() {
	bytePositions := parseInput("./day18/inputs/input.txt")
	start := Position{x: 0, y: 0}
	end := Position{x: 70, y: 70}
	simulationLength := 1024

	pathLength, _ := findPath(bytePositions[:simulationLength], start, end)

	println(pathLength)
}

func Part2() {
	bytePositions := parseInput("./day18/inputs/input.txt")
	start := Position{x: 0, y: 0}
	end := Position{x: 70, y: 70}
	simulationLength := 1024

	found := true
	for ; found; simulationLength++ {
		_, found = findPath(bytePositions[:simulationLength], start, end)
	}

	blockingByte := bytePositions[simulationLength-2] // Go increments the loop variable one last time after the condition is met, so we have to decrement 2 here instead of 1
	print(blockingByte.x)
	print(",")
	print(blockingByte.y)
}

func findPath(bytePositions []Position, start Position, end Position) (pathLength int, found bool) {
	open := []Node{}
	closed := []Node{}

	open = append(open, Node{position: start, estimatedCost: manhattanDistance(start, end)})
	for len(open) > 0 {
		sort.Slice(open, func(i, j int) bool { return open[i].estimatedCost < open[j].estimatedCost })
		node := open[0]

		if node.position == end {
			found = true
			pathLength = 0
			head := node
			for head.from != nil {
				pathLength++
				head = *head.from
			}
			return
		}

	directionsLoop:
		for _, direction := range directions {
			neighborPosition := Position{x: node.position.x + direction.deltaX, y: node.position.y + direction.deltaY}
			updatedEstimation := node.cost + manhattanDistance(neighborPosition, end)

			// If out of bounds, skip
			if neighborPosition.x < 0 || neighborPosition.x > end.x || neighborPosition.y < 0 || neighborPosition.y > end.y {
				continue
			}

			// If it's corrupted, skip
			for _, position := range bytePositions {
				if neighborPosition == position {
					continue directionsLoop
				}
			}

			// If a better path was found, skip
			found, neighbor := find(neighborPosition, open)
			if found && updatedEstimation >= neighbor.estimatedCost {
				continue
			}

			// If already explored, skip
			isClosed, _ := find(neighborPosition, closed)
			if isClosed {
				continue
			}

			neighbor.position = neighborPosition
			neighbor.cost = node.cost + 1
			neighbor.estimatedCost = updatedEstimation
			neighbor.from = &node
			open = append(open, neighbor)
		}

		open = open[1:]
		closed = append(closed, node)
	}

	return
}

func manhattanDistance(left, right Position) int {
	return abs(left.x-right.x) + abs(left.y-right.y)
}

func abs(value int) int {
	if value >= 0 {
		return value
	}

	return -value
}

func find(position Position, nodes []Node) (found bool, node Node) {
	for _, candidate := range nodes {
		if candidate.position == position {
			return true, candidate
		}
	}
	return false, Node{}
}
