
from functools import reduce
from typing import Iterable

inp = [[int(c) for c in s.strip()] for s in open('../../inputs/03.txt').readlines()]

def to_dec(l: Iterable[int]) -> int:
  return reduce(lambda a, c: a * 2 + c, l, 0)

p1gamma = to_dec(map(lambda i: sum(map(lambda n: n[i], inp)) >= len(inp) / 2, range(len(inp[0]))))
p1epsilon = ~p1gamma & (2 ** len(inp[0]) - 1)

print(p1gamma * p1epsilon)

