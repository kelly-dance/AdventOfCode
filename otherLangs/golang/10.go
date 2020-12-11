package main

import (
	"fmt"
	"io/ioutil"
	"sort"
	"strconv"
	"strings"
)

func helper(cache *[]int, input *[]int, index int) int {
	if (*cache)[index] != 0 {
		return (*cache)[index]
	}
	n := (*input)[index]
	score := 0
	for i := index + 1; i < len(*input) && (*input)[i]-n <= 3; i++ {
		score += helper(cache, input, i)
	}
	(*cache)[index] = score
	return score
}

func main() {

	content, _ := ioutil.ReadFile("inputs/10.txt")
	text := string(content)
	s := strings.Split(text, "\r\n")
	nums := make([]int, len(s)+2)
	nums[0] = 0
	for i := 0; i < len(s); i++ {
		parsed, err := strconv.ParseInt(s[i], 10, 64)
		if err != nil {
			panic(err)
		}
		nums[i] = int(parsed)
	}
	var slice []int = nums[0 : len(nums)-1]
	sort.Ints(slice)
	nums[len(nums)-1] = slice[len(nums)-2] + 3
	ones := 0
	threes := 0
	for i := 1; i < len(nums); i++ {
		dif := nums[i] - nums[i-1]
		if dif == 1 {
			ones++
		}
		if dif == 3 {
			threes++
		}
	}
	fmt.Println(ones * threes)
	cache := make([]int, len(nums))
	cache[len(nums)-1] = 1
	part2 := helper(&cache, &nums, 0)
	fmt.Println(part2)
}
