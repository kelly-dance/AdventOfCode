package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func main() {

	content, _ := ioutil.ReadFile("inputs/01.txt")
	text := string(content)
	s := strings.Split(text, "\n")
	nums := make([]int, len(s))
	var parsed int64
	var err error
	for i := 0; i < len(s); i++ {
		parsed, err = strconv.ParseInt(s[i], 10, 64)
		if err != nil {
			panic(err)
		}
		nums[i] = int(parsed)
	}
	for i := 0; i < len(nums); i++ {
		for j := i + 1; j < len(nums); j++ {
			if nums[i]+nums[j] == 2020 {
				fmt.Println("Part 1:")
				fmt.Println(nums[i] * nums[j])
			}
			for k := j + 1; k < len(nums); k++ {
				if nums[i]+nums[j]+nums[k] == 2020 {
					fmt.Println("Part 2:")
					fmt.Println(nums[i] * nums[j] * nums[k])
				}
			}
		}
	}
}
