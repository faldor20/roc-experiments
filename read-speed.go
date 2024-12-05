package main

import (
    "bufio"
    // "fmt"
    "io"
    "log"
    "os"
    // "time"
)
func main() {
    file, err := os.Open("input.txt")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

    reader := bufio.NewReader(file)

    var total int64
    buffer := make([]byte, 8000) // 32KB buffer

    // start := time.Now()

    for {
        n, err := reader.Read(buffer)
        if err == io.EOF {
            break
        }
        if err != nil {
            log.Fatal(err)
        }
        total += int64(n)
    }

    // elapsed := time.Since(start)
    // throughput := float64(total) / elapsed.Seconds() / 1024 / 1024 // MB/s

    // fmt.Printf("Total bytes read: %d\n", total)
    // fmt.Printf("Time taken: %v\n", elapsed)
    // fmt.Printf("Throughput: %.2f MB/s\n", throughput)
}
