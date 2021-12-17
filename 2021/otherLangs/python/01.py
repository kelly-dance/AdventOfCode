from functools import reduce

inp = [int(x) for x in open('../../inputs/01.txt').readlines()]

print(reduce(lambda a, t: a + 1 if t[0] < t[1] else a, zip(inp, inp[1:]), 0))
print(reduce(lambda a, t: a + 1 if t[0] < t[1] else a, zip(inp, inp[3:]), 0))
