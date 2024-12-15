package day14

import (
	"bufio"
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

type Vector struct {
	X int
	Y int
}

type Robot struct {
	Position Vector
	Velocity Vector
}

func parseInput(fileName string) (robots []Robot) {
	file, err := os.Open(fileName)
	check(err)
	defer file.Close()

	inputBytes, _ := io.ReadAll(file)
	input := string(inputBytes)
	input = strings.ReplaceAll(input, "\r\n", "\n")

	for _, machine := range strings.Split(input, "\n") {
		parameters := strings.Split(machine, " ")

		position := parseVector(strings.Trim(parameters[0], "p="))
		velocity := parseVector(strings.Trim(parameters[1], "v="))

		robots = append(robots, Robot{
			Position: position,
			Velocity: velocity,
		})
	}

	return
}

func parseVector(instruction string) (vector Vector) {
	values := strings.Split(instruction, ",")
	xValue, _ := strconv.Atoi(values[0])
	yValue, _ := strconv.Atoi(values[1])

	vector = Vector{X: xValue, Y: yValue}
	return
}

func Part1() {
	robots := parseInput("./day14/inputs/input.txt")
	dimensions := Vector{X: 101, Y: 103}

	firstQuadrant := 0
	secondQuadrant := 0
	thirdQuadrant := 0
	fourthQuadrant := 0
	seconds := 100
	for _, robot := range robots {
		p := move(robot, seconds)
		p.X = (p.X + seconds*dimensions.X) % dimensions.X // Add the dimension (0 mod dimension) to get rid of negatives
		p.Y = (p.Y + seconds*dimensions.Y) % dimensions.Y

		switch {
		case p.X < (dimensions.X/2) && p.Y < (dimensions.Y/2):
			firstQuadrant++
		case p.X > (dimensions.X/2) && p.Y < (dimensions.Y/2):
			secondQuadrant++
		case p.X < (dimensions.X/2) && p.Y > (dimensions.Y/2):
			thirdQuadrant++
		case p.X > (dimensions.X/2) && p.Y > (dimensions.Y/2):
			fourthQuadrant++
		}
	}

	fmt.Println(firstQuadrant, secondQuadrant, thirdQuadrant, fourthQuadrant)
	safetyFactor := firstQuadrant * secondQuadrant * thirdQuadrant * fourthQuadrant
	fmt.Println(safetyFactor)
}

func Part2() {
	robots := parseInput("./day14/inputs/input.txt")
	dimensions := Vector{X: 101, Y: 103}

	seconds := 1
	for iteration := 1; iteration < 10000; iteration++ {
		for robot := 0; robot < len(robots); robot++ {
			robots[robot].Position = move(robots[robot], seconds)
			robots[robot].Position.X = (robots[robot].Position.X + seconds*dimensions.X) % dimensions.X // Add the dimension (0 mod dimension) to get rid of negatives
			robots[robot].Position.Y = (robots[robot].Position.Y + seconds*dimensions.Y) % dimensions.Y
		}

		picture := make([][]rune, dimensions.Y)
		for i := 0; i < dimensions.Y; i++ {
			picture[i] = make([]rune, dimensions.X)
			for j := 0; j < dimensions.X; j++ {
				picture[i][j] = '.'
			}
		}

		for _, robot := range robots {
			picture[robot.Position.Y][robot.Position.X] = '#'
		}

		candidate := false
		for _, row := range picture {
			s := string(row)
			candidate = strings.Contains(s, "##########")
			if candidate {
				break
			}
		}

		if candidate {
			for _, row := range picture {

				for _, char := range row {
					print(string(char))
				}
				println()
			}
			fmt.Println("Iteration", iteration)

			reader := bufio.NewReader(os.Stdin)
			reader.ReadString('\n')
		}
	}
}

func move(robot Robot, seconds int) (position Vector) {
	position.X = robot.Position.X + seconds*robot.Velocity.X
	position.Y = robot.Position.Y + seconds*robot.Velocity.Y
	return
}
