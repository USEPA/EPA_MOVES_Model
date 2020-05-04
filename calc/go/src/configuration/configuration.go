package configuration

import (
	"fmt"
)

type Configuration struct {
	isReady bool
	maximumMemory int64
}

var Singleton Configuration

func init() {
	// Nothing to do here
}

func (c Configuration) Setup() {
	fmt.Println("Reading the global configuration values...")
	c.isReady = false
	c.maximumMemory = 1024*1024*1024 // 1GB
	c.maximumMemory *= 4 // 4GB

	// TODO
	fmt.Println("Done reading the global configuration values.")
}
