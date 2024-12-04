package day04

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func parseInput(fileName string) [][]string {
	file, err := os.Open(fileName)
	check(err)
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var grid [][]string
	for scanner.Scan() {
		line := scanner.Text()
		lineLetters := strings.Split(line, "")
		grid = append(grid, lineLetters)
	}

	return grid
}

func Part1() {
	grid := parseInput("./day04/inputs/input.txt")
	maxRows := len(grid) - 1
	maxColums := len(grid[0]) - 1
	directions := []string{"up", "upright", "right", "downright", "down", "downleft", "left", "upleft"}

	count := 0
	for row, letters := range grid {
		for column, letter := range letters {
			if letter == "X" {
				for _, direction := range directions {
					if searchWord(grid, column, row, direction, maxColums, maxRows) {
						count++
					}
				}
			}
		}
	}

	fmt.Println(count)
}

func Part2() {
	grid := parseInput("./day04/inputs/input.txt")
	maxRows := len(grid) - 1
	maxColums := len(grid[0]) - 1

	count := 0
	for row, letters := range grid {
		for column, letter := range letters {
			if letter == "A" {
				found := searchXMas(grid, column, row, maxColums, maxRows)
				if found {
					count++
				}
			}
		}
	}

	fmt.Println(count)
}

func searchXMas(grid [][]string, column int, row int, maxColumn int, maxRow int) bool {
	directions := []string{"upleft", "upright", "downright", "downleft"}

	for _, direction := range directions {
		overflow := checkOverflow(direction, row, maxRow, column, maxColumn)
		if overflow {
			return false
		}
	}

	nextColumn, nextRow := getUpdatedIndexes(column, row, "upleft")
	upLeft := grid[nextRow][nextColumn]

	nextColumn, nextRow = getUpdatedIndexes(column, row, "downright")
	downRight := grid[nextRow][nextColumn]

	nextColumn, nextRow = getUpdatedIndexes(column, row, "upright")
	upRight := grid[nextRow][nextColumn]

	nextColumn, nextRow = getUpdatedIndexes(column, row, "downleft")
	downLeft := grid[nextRow][nextColumn]

	if ((upLeft == "M" && downRight == "S") || (upLeft == "S" && downRight == "M")) &&
		((upRight == "M" && downLeft == "S") || (upRight == "S" && downLeft == "M")) {
		return true
	}

	return false
}

func searchWord(grid [][]string, column int, row int, direction string, maxColumn int, maxRow int) bool {
	currentLetter := grid[row][column]
	if currentLetter == "S" {
		return true
	}

	overflow := checkOverflow(direction, row, maxRow, column, maxColumn)
	if overflow {
		return false
	}

	nextColumn, nextRow := getUpdatedIndexes(column, row, direction)

	nextLetter := grid[nextRow][nextColumn]
	if (currentLetter == "X" && nextLetter == "M") ||
		(currentLetter == "M" && nextLetter == "A") ||
		(currentLetter == "A" && nextLetter == "S") {
		return searchWord(grid, nextColumn, nextRow, direction, maxColumn, maxRow)
	}

	return false
}

func checkOverflow(direction string, row int, maxRow int, column int, maxColumn int) bool {
	if (strings.Contains(direction, "up") && row <= 0) ||
		(strings.Contains(direction, "down") && row >= maxRow) ||
		(strings.Contains(direction, "left") && column <= 0) ||
		(strings.Contains(direction, "right") && column >= maxColumn) {
		return true
	}

	return false
}

func getUpdatedIndexes(column int, row int, direction string) (int, int) {
	nextColumn := column
	nextRow := row
	if strings.Contains(direction, "up") {
		nextRow--
	}

	if strings.Contains(direction, "down") {
		nextRow++
	}

	if strings.Contains(direction, "left") {
		nextColumn--
	}

	if strings.Contains(direction, "right") {
		nextColumn++
	}

	return nextColumn, nextRow
}
