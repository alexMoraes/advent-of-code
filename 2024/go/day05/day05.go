package day05

import (
	"fmt"
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

func parseInput(fileName string) ([]string, []string) {
	file, err := os.Open(fileName)
	check(err)
	defer file.Close()

	inputBytes, _ := io.ReadAll(file)
	input := string(inputBytes)

	sanitizedInput := strings.ReplaceAll(input, "\r\n", "\n")
	parts := strings.Split(sanitizedInput, "\n\n")

	rules := strings.Split(parts[0], "\n")
	updateList := parts[1]
	updates := strings.Split(updateList, "\n")

	return rules, updates
}

func Part1() {
	rules, updates := parseInput("./day05/inputs/input.txt")

	var correctUpdates []string
	for _, update := range updates {
		pages := strings.Split(update, ",")
		isCorrect := sort.SliceIsSorted(pages, func(i, j int) bool {
			return isAscending(pages[i], pages[j], rules)
		})

		if isCorrect {
			correctUpdates = append(correctUpdates, update)
		}
	}

	sum := 0
	for _, update := range correctUpdates {
		pages := strings.Split(update, ",")
		index := (len(pages) - 1) / 2
		value, _ := strconv.Atoi(pages[index])
		sum += value
	}

	fmt.Println(sum)
}

func Part2() {
	rules, updates := parseInput("./day05/inputs/input.txt")

	var incorrectUpdates []string
	for _, update := range updates {
		pages := strings.Split(update, ",")
		isCorrect := sort.SliceIsSorted(pages, func(i, j int) bool {
			return isAscending(pages[i], pages[j], rules)
		})

		if !isCorrect {
			incorrectUpdates = append(incorrectUpdates, update)
		}
	}

	sum := 0
	for _, update := range incorrectUpdates {
		pages := strings.Split(update, ",")
		sort.Slice(pages, func(i, j int) bool {
			return isAscending(pages[i], pages[j], rules)
		})

		index := (len(pages) - 1) / 2
		value, _ := strconv.Atoi(pages[index])
		sum += value
	}

	fmt.Println(sum)
}

func isAscending(left, right string, rules []string) bool {
	ruleExists := false
	for _, rule := range rules {
		if rule == left+"|"+right {
			ruleExists = true
			break
		}
	}
	return ruleExists
}
