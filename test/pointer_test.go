package main

import (
	"testing"
)

type Car struct {
	Type string
	Age  int
}

func TestPointer(t *testing.T) {
	car1 := &Car{"BMW", 1}
	t.Fatal(car1.Type)
}
