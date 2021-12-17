inp = [(s.split()[0], int(s.split()[1])) for s in open('../../inputs/02.txt').readlines()]

# commented out because unsupported match statements
# make mypy freak out and die

# # part 1
# part1pos = 0
# part1depth = 0
# for inst, param in inp:
#   match inst:
#     case 'up':
#       part1depth -= param
#     case 'down':
#       part1depth += param
#     case 'forward':
#       part1pos += param

# print(part1pos * part1depth)

# # part 2
# part2pos = 0
# part2aim = 0
# part2depth = 0
# for inst, param in inp:
#   match inst:
#     case 'up':
#       part2aim -= param
#     case 'down':
#       part2aim += param
#     case 'forward':
#       part2pos += param
#       part2depth += param * part2aim

# print(part2pos * part2depth)


