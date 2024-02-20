package main

import (
	"fmt"
	"interpreter/repl"
	"os"
	"os/user"
)

func main() {
	_, err := user.Current()
	if err != nil {
		panic(err)
	}
	fmt.Println("This is monkey PL")
	repl.Start(os.Stdin, os.Stdout)
}
