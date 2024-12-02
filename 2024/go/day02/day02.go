package day02

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func Map[T any, U any](input []T, transform func(T) U) []U {
	result := make([]U, len(input))
	for i, v := range input {
		result[i] = transform(v)
	}
	return result
}

func parseInput(fileName string) [][]int {
	file, err := os.Open(fileName)
	check(err)
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var reports [][]int
	for scanner.Scan() {
		line := scanner.Text()
		reportStr := strings.Split(line, " ")
		report := Map(reportStr, func(s string) int {
			value, _ := strconv.Atoi(s)
			return value
		})

		reports = append(reports, report)
	}

	return reports
}

func Part1() {
	reports := parseInput("./day02/inputs/input.txt")

	var count int
	for _, report := range reports {
		isSafe := isReportSafe(report)

		if isSafe {
			count++
		}
	}

	fmt.Println(count)
}

func Part2() {
	reports := parseInput("./day02/inputs/input.txt")

	var count int
	for _, report := range reports {
		isSafe := isReportSafe(report)

		if !isSafe {
			for skipped := range report {
				reportCopy := append([]int{}, report[:skipped]...)
				dampenedReport := append(reportCopy, report[skipped+1:]...)
				isSafe = isReportSafe(dampenedReport)

				if isSafe {
					break
				}
			}
		}

		if isSafe {
			count++
		}
	}

	fmt.Println(count)
}

func isReportSafe(report []int) bool {
	direction := 0

	isSafe := true

	for i := 0; i < len(report)-1; i++ {
		level := report[i]
		nextLevel := report[i+1]
		levelDifference := level - nextLevel

		if (levelDifference > 0 && direction < 0) || (levelDifference < 0 && direction > 0) || levelDifference == 0 || levelDifference > 3 || levelDifference < -3 {
			isSafe = false
			break
		}

		direction = levelDifference
	}

	return isSafe
}
