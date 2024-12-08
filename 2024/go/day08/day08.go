package day08

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

type Point struct {
	Row    int
	Column int
}

func parseInput(fileName string) (antennas map[rune][]Point, gridLimit Point) {
	file, err := os.Open(fileName)
	check(err)
	defer file.Close()

	inputBytes, _ := io.ReadAll(file)
	input := string(inputBytes)
	input = strings.ReplaceAll(input, "\r\n", "\n")

	antennas = map[rune][]Point{}
	rows := strings.Split(input, "\n")
	for rowIndex, row := range rows {
		for columnIndex, point := range row {
			if point != '.' && point != '#' {
				if antennas[point] == nil {
					antennas[point] = []Point{}
				}
				antennas[point] = append(antennas[point], Point{Row: rowIndex, Column: columnIndex})
			}
			gridLimit = Point{Row: rowIndex, Column: columnIndex}
		}
	}

	return
}

func Part1() {
	antennas, gridLimit := parseInput("./day08/inputs/input.txt")

	antinodes := map[Point]bool{}
	for _, frequency := range antennas {
		for _, antenna := range frequency {
			for _, secondAntenna := range frequency {
				if secondAntenna == antenna {
					continue
				}

				deltaRow := secondAntenna.Row - antenna.Row
				deltaColumn := secondAntenna.Column - antenna.Column

				point := Point{Row: secondAntenna.Row + deltaRow, Column: secondAntenna.Column + deltaColumn}
				if point.Row <= gridLimit.Row && point.Row >= 0 && point.Column <= gridLimit.Column && point.Column >= 0 {
					antinodes[point] = true
				}
			}
		}
	}

	fmt.Println(len(antinodes))
}

func Part2() {
	antennas, gridLimit := parseInput("./day08/inputs/input.txt")

	antinodes := map[Point]bool{}
	for _, frequency := range antennas {
		for _, antenna := range frequency {
			for _, secondAntenna := range frequency {
				if secondAntenna == antenna {
					continue
				}

				deltaRow := secondAntenna.Row - antenna.Row
				deltaColumn := secondAntenna.Column - antenna.Column

				point := Point{Row: antenna.Row, Column: antenna.Column}
				for point.Row <= gridLimit.Row && point.Row >= 0 && point.Column <= gridLimit.Column && point.Column >= 0 {
					antinodes[point] = true
					point.Row += deltaRow
					point.Column += deltaColumn
				}
			}
		}
	}

	fmt.Println(len(antinodes))
}
