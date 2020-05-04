package parse

import (
	"strconv"
	"bufio"
	"io"
	"os"
	"fmt"
	"strings"
)

var ShouldComplain bool
var complaintCount int
const maxComplaintCount = 10

func init() {
	ShouldComplain = true
}

func IsNull(t string) bool {
	return t == "\\N"
}

func GetInt(t string) int {
	if IsNull(t) {
		return 0
	}
	result, err := strconv.ParseInt(t,10,0)
	if err != nil {
		if ShouldComplain && complaintCount < maxComplaintCount {
			complaintCount++
			fmt.Println(err)
		}
		return 0
	}
	return int(result)
}

func GetString(t string) string {
	if IsNull(t) {
		return ""
	}
	return t
}

func GetFloat(t string) float64 {
	if IsNull(t) {
		return 0
	}
	result, err := strconv.ParseFloat(t,64)
	if err != nil {
		if ShouldComplain && complaintCount < maxComplaintCount {
			complaintCount++
			fmt.Println(err)
		}
		return 0
	}
	return result
}

func GetBool(t string) bool {
	s := GetString(t)
	return s == "Y" || s == "y" || s == "1" || s == "T" || s == "t"
}

func ReadAndParseFile(fileName string, parseFunc func([]string)) {
	//fmt.Println("Reading from: ",fileName)
	inputFile, inputError := os.Open(fileName)
	if inputError != nil {
		fmt.Printf("An error occurred on opening the file %s\n", fileName)
		return // exit the function on error
	}
	defer inputFile.Close()
	inputReader := bufio.NewReader(inputFile)
	for {
		inputString, readerError := inputReader.ReadString('\n')
		if readerError == io.EOF {
			break
		}
		inputString = strings.TrimSuffix(inputString,"\n")
		inputString = strings.TrimSuffix(inputString,"\r")
		if len(inputString) > 0 {
			parts := strings.Split(inputString,"\t")
			if parts != nil {
				parseFunc(parts)
			}
		}
	}
	//fmt.Println("Done reading from file: ",fileName)
}
