// Package configuration establishes the runtime environment, validates
// and completes the input parameters, opens the execution database.
package configuration

import (
	"flag"
	"fmt"
  "strings"

	_ "github.com/go-sql-driver/mysql"
	"database/sql"
	
	// used to read MySQL.txt 
	"os"
	"io/ioutil"
)

//type Configuration struct creates a structure encompassing the
//startup for the current run.
type Configuration struct {
	IsReady bool
	MaximumMemory int64
	ClassName string
	ExecutionDatabase string
	DBUser, DBPassword, Host, Port string
	RawParameters string
	Parameters []string
	IsProject bool
	SaveROMD bool
}

// The one and only Configuration object in existence
var Singleton Configuration

// init performs package-level initialization.
// Currently, there is nothing to initialize.
func init() {
	// Nothing to do here
}

// Setup reads the command line parameters, storing their values into
// an existing Configuration object.
func (c *Configuration) Setup() {
	fmt.Println("Reading the global configuration values...")
	c.IsReady = false
	c.MaximumMemory = 1024 * 1024 * 1024 // 1GB
	c.MaximumMemory *= 4 // 4GB
	c.Host = "localhost"
	c.Port = "3306" // This is the default port
	
	// Read the MySQL.txt file for the port number
	wd, _ := os.Getwd()
	fmt.Println("Generator configuration running from:", wd)
	// For now, we'll assume the MySQL.txt only has a port number in it
	//  we can get an error for a variety of reasons, but the most common is that the file doesn't exist
	//  If it doesn't exist, that's not a problem, it just means that we use the standard port
	// note: the filename is NOT case sensitive
	newPortBytes, err := ioutil.ReadFile("../../MySQL.txt")
	if err != nil {
		fmt.Println("Error reading MySQL.txt (this may not necessarily be a problem):", err)
	} else {
		// only set the new port if there's no error
		// make sure to trim possible newlines and whitespaces
		newPort := string(newPortBytes)
		newPort = strings.Trim(newPort, "\n")
		newPort = strings.TrimSpace(newPort)
		c.Port = newPort
	}

	classArg := flag.String("class", "classname", "Name of module to be run")
	databaseArg := flag.String("db", "executiondbname", "Name of MOVES Execution MySQL database")
	dbUserArg := flag.String("dbuser", "username", "MySQL user name")
	dbPasswordArg := flag.String("dbpwd", "password", "MySQL password")
	paramsArg := flag.String("params", "parameters", "Optional context parameters")
	projectArg := flag.String("isproject", "isproject", "Optional project flag")
	saveROMDArg := flag.String("saveromd", "saveROMD", "Optional flag to save ROMD data")

	flag.Parse()
	c.ClassName = *classArg
	c.ExecutionDatabase = *databaseArg
	c.DBUser = *dbUserArg
	c.DBPassword = *dbPasswordArg
	c.RawParameters = *paramsArg
	c.Parameters = strings.Split(c.RawParameters,",")
	if c.Parameters != nil && len(c.Parameters) > 0 {
		for i:=0;i<len(c.Parameters);i++ {
			c.Parameters[i] = strings.TrimSpace(c.Parameters[i])
		}
	}
	c.IsProject = *projectArg == "1"
	c.SaveROMD = *saveROMDArg == "1"

	fmt.Println("Class name to execute:",c.ClassName)
	fmt.Println("Execution parameters:",c.RawParameters)
	fmt.Println("Execution database:",c.ExecutionDatabase)
	fmt.Println("Using MySQL user:",c.DBUser)
	fmt.Println("Connecting to MySQL on:",c.Port)
	// Don't print the password
	fmt.Println("IsProject:",c.IsProject)
	fmt.Println("SaveROMD:",c.SaveROMD)

	fmt.Println("Done reading the global configuration values.")
}

// OpenInputDatabase opens the execution database and confirms it is accessible.
func OpenExecutionDatabase() *sql.DB {
	fmt.Println("Opening execution database connection...")
	//connectionString := configuration.Singleton.DBUser + ":" + configuration.Singleton.DBPassword + "@/" + configuration.Singleton.InputDatabase + "?charset=utf8"
	connectionString := Singleton.DBUser + ":" + Singleton.DBPassword + "@tcp(" + Singleton.Host + ":" + Singleton.Port + ")/" + Singleton.ExecutionDatabase
	db, err := sql.Open("mysql", connectionString)
	CheckErr(err)
	fmt.Println("Doing database ping to verify execution database connection...")
	err = db.Ping()
	CheckErr(err)
	return db
}

// CheckErr terminates the program upon an error
func CheckErr(err error) {
	if err != nil {
		fmt.Fprintln(os.Stderr, "ERROR in Go generator, causing a panic:", err)
		panic(err)
	}
}

// TestInputDatabase opens the input database and tests reading rows from the nrAge table.
// This function also serves as a template for database reading logic.
func TestExecutionDatabase() {
	// https://astaxie.gitbooks.io/build-web-application-with-golang/content/en/05.2.html
	db := OpenExecutionDatabase()
	defer db.Close()

	fmt.Println("Running query in execution database...")
	rows, err := db.Query("select ageID, ageGroupID from ageCategory")
	CheckErr(err)

    defer rows.Close()
    for rows.Next() {
		var ageID int
		var ageGroupID int
        err = rows.Scan(&ageID, &ageGroupID)
		CheckErr(err)
        //fmt.Println(ageID)
    }
	fmt.Println("Execution database query succeeded.")
}
