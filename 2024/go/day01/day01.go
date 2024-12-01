package day01

import (
	"bufio"
	"fmt"
	"os"
	"slices"
	"strconv"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func parseInput(fileName string) ([]int, []int) {
	file, err := os.Open(fileName)
	check(err)
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var firstList []int
	var secondList []int

	for scanner.Scan() {
		line := scanner.Text()
		entries := strings.Split(line, "   ")

		firstEntry, _ := strconv.Atoi(entries[0])
		secondEntry, _ := strconv.Atoi(entries[1])

		firstList = append(firstList, firstEntry)
		secondList = append(secondList, secondEntry)
	}

	slices.Sort(firstList)
	slices.Sort(secondList)

	return firstList, secondList
}

func Part1() {
	var firstList, secondList = parseInput("./day01/inputs/input.txt")

	var sum int
	for i := range firstList {
		var distance int = firstList[i] - secondList[i]

		if distance < 0 {
			distance = -distance
		}

		sum += distance
	}

	fmt.Println(sum)
}

func Part2() {
	firstList, secondList := parseInput("./day01/inputs/input.txt")

	similarityScores := make(map[int]int)
	currentValue := secondList[0]
	similarityScore := 0
	for _, rightValue := range secondList {
		if rightValue != currentValue {
			similarityScores[currentValue] = similarityScore
			currentValue = rightValue
			similarityScore = 0
		}

		similarityScore++
	}

	var sum int
	for _, firstValue := range firstList {
		sum += firstValue * similarityScores[firstValue]
	}

	fmt.Println(sum)
}
