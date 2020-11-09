// SQL queue and support functions
// @author Wesley Faler
// @version 2016-03-06
package sqlutility

import (
	"fmt"

	"gen/configuration"
	"gen/globalevents"
)

// SQL statements waiting to be executed by the database.
var finalSqlToWrite []string

// StartWriting creates the output threads to handle writing
// the activity and pollution data to the output database.
func StartWriting(howManyThreads int, sqlToWrite chan string) {
	finalSqlToWrite = make([]string,1000)

	accumulatorIndex := 0

	for i := 0; i<howManyThreads; i++ {
		go writeSql(accumulatorIndex,sqlToWrite)
		accumulatorIndex++
	}
}

// Add SQL to be executed after all other pending SQL has been completed.
func AddFinalSql(sql string) {
	if len(sql) > 0 {
		finalSqlToWrite = append(finalSqlToWrite,sql)
	}
}

// FinishWriting executes all pending accumulated SQL statements and removes temporary tables.
func FinishWriting() {
	fmt.Println("Executing final SQL...")
	db := configuration.OpenExecutionDatabase()
	defer db.Close()

	for _, sql := range finalSqlToWrite {
		if len(sql) > 0 {
			_, err := db.Exec(sql)
			configuration.CheckErr(err)
		}
	}
	finalSqlToWrite = nil
	fmt.Println("Final SQL done.")
}

// Execute pending SQL statements.
func writeSql(accumulatorIndex int, sqlToWrite chan string) {
	db := configuration.OpenExecutionDatabase()
	defer db.Close()

	for {
		sql := <- sqlToWrite

		//fmt.Println(sql)
		_, err := db.Exec(sql)
		configuration.CheckErr(err)

		globalevents.SqlDone()
	}
}
