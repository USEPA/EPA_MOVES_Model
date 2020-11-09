// Helper routines to parse data files.
package parse

import (
	"strconv"
	"bufio"
	"io"
	"os"
	"fmt"
	"strings"
)

// true when error messages should be written.
var ShouldComplain bool

// Number of error messages written.
var complaintCount int

// Maximum number of error messages that will be written before errors are disabled.
const maxComplaintCount = 10

// Initialize variables.
func init() {
	ShouldComplain = true
}

// true when the MySQL text "\N" is present.
func IsNull(t string) bool {
	return t == "\\N"
}

// Convert text to an integer, returing 0 for IsNull() strings.
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

// Convert text to a string, returing "" for IsNull() strings.
func GetString(t string) string {
	if IsNull(t) {
		return ""
	}
	return t
}

// Convert text to a 64-bit float, returing 0 for IsNull() strings.
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

// Convert text to a boolean value.
// true values: Y, y, 1, T, t.
// All other values are false.
func GetBool(t string) bool {
	s := GetString(t)
	return s == "Y" || s == "y" || s == "1" || s == "T" || s == "t"
}

// Read a text file, calling a parsing lambda function for each data line.
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
