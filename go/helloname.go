package main

import (
    "bufio"
    "fmt"
    "os"
)

func main() {
    r := bufio.NewReader(os.Stdin)
    name, _, err := r.ReadLine()
    if err != nil {
        fmt.Println("error")
    }
    fmt.Printf("Hello, %s!\n", name)
}
