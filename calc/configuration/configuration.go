/*
Read and hold configuration data for the program.
@author Wesley Faler
@version 2016-01-13
*/
package configuration

import (
	"fmt"
)

// Holds the settings for all configurable options
type Configuration struct {
	isReady bool
	maximumMemory int64
}

// The one and only repository of configured values
var Singleton Configuration

// Initialize package-level variables.
func init() {
	// Nothing to do here
}

// Read the values for all configurable options into a Configuration object.
func (c *Configuration) Setup() {
	fmt.Println("Reading the global configuration values...")
	c.isReady = false
	c.maximumMemory = 1024*1024*1024 // 1GB
	c.maximumMemory *= 4 // 4GB

	// TODO
	fmt.Println("Done reading the global configuration values.")
}
