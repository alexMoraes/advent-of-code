package day09

import (
	"io"
	"maps"
	"os"
	"slices"
	"sort"
	"strconv"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

const FILE uint8 = 0
const FREE uint8 = 1

type Sector struct {
	Start int
	End   int
}

func parseInput(fileName string) (files map[int][]Sector, freeSectors []Sector) {
	file, err := os.Open(fileName)
	check(err)
	defer file.Close()

	inputBytes, _ := io.ReadAll(file)
	input := string(inputBytes)
	input = strings.ReplaceAll(input, "\r\n", "\n")

	sectorType := FILE
	sectorStart := 0
	freeSectors = []Sector{}
	fileIndex := 0
	files = map[int][]Sector{}
	for _, s := range input {
		size, _ := strconv.Atoi(string(s))
		sector := Sector{Start: sectorStart, End: sectorStart + size}

		switch sectorType {
		case FREE:
			freeSectors = append(freeSectors, sector)
		case FILE:
			files[fileIndex] = []Sector{sector}
			fileIndex++
		}

		sectorType = (sectorType + 1) % 2
		sectorStart += size
	}

	return
}

func Part1() {
	files, freeSectors := parseInput("./day09/inputs/input.txt")

	fileIndexes := slices.Collect(maps.Keys(files))
	sort.Slice(fileIndexes, func(i, j int) bool { return fileIndexes[i] > fileIndexes[j] })

	lastFileIndex := fileIndexes[0]
	lastUsedFreeSector := len(freeSectors)
	for i := 0; i < lastUsedFreeSector; {
		freeSectorSize := freeSectors[i].End - freeSectors[i].Start

		// If current free space is used up, move to the next one
		if freeSectorSize <= 0 {
			i++
			continue
		}

		fileSectorSize := files[lastFileIndex][0].End - files[lastFileIndex][0].Start
		switch {
		case fileSectorSize <= freeSectorSize:
			files[lastFileIndex] = append(files[lastFileIndex], Sector{Start: freeSectors[i].Start, End: freeSectors[i].Start + fileSectorSize})
			files[lastFileIndex] = files[lastFileIndex][1:]
			lastFileIndex--
			lastUsedFreeSector--

		case fileSectorSize > freeSectorSize:
			files[lastFileIndex] = append(files[lastFileIndex], Sector{Start: freeSectors[i].Start, End: freeSectors[i].End})
			files[lastFileIndex][0].End -= freeSectorSize
		}

		freeSectors[i].Start += fileSectorSize
	}

	sum := 0
	for fileIndex, file := range files {
		for _, sector := range file {
			for block := sector.Start; block < sector.End; block++ {
				sum += fileIndex * block
			}
		}
	}

	println(sum)
}

func Part2() {
	files, freeSectors := parseInput("./day09/inputs/input.txt")

	fileIndexes := slices.Collect(maps.Keys(files))
	sort.Slice(fileIndexes, func(i, j int) bool { return fileIndexes[i] > fileIndexes[j] })

	for lastFileIndex := fileIndexes[0]; lastFileIndex >= 0; lastFileIndex-- {
		for i := 0; i < lastFileIndex; i++ {
			freeSectorSize := freeSectors[i].End - freeSectors[i].Start
			fileSectorSize := files[lastFileIndex][0].End - files[lastFileIndex][0].Start

			if fileSectorSize <= freeSectorSize {
				files[lastFileIndex] = []Sector{{Start: freeSectors[i].Start, End: freeSectors[i].Start + fileSectorSize}}
				freeSectors[i].Start += fileSectorSize
				break
			}
		}
	}

	sum := 0
	for fileIndex, file := range files {
		for block := file[0].Start; block < file[0].End; block++ {
			sum += fileIndex * block
		}
	}

	println(sum)
}
