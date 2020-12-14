import { mod, readFile } from './tools.ts';

const input = readFile('inputs/12.txt').split('\r\n').map(l => {
  let [instruction, arg] =  [l.charAt(0), parseInt(l.substring(1))];
  if(instruction === 'R') arg = mod(arg/90,4);
  else if(instruction === 'L') arg = mod(arg/-90,4);
  return [instruction, arg]
});

const directions = [
  [1,0],
  [0,1],
  [-1,0],
  [0,-1],
]

const part1 = input.reduce(({direction, position}, [instruction, arg]) => {
  switch (instruction) {
    case 'L':
    case 'R':
      direction = mod((direction + arg), 4)
      break;
    case 'N':
    case 'E':
    case 'S':
    case 'W':
      const dir = {N:0,E:1,S:2,W:3}[instruction]
      position[0] += arg * directions[dir][0];
      position[1] += arg * directions[dir][1];
      break;
    case 'F':
      position[0] += arg * directions[direction][0];
      position[1] += arg * directions[direction][1];
      break;
  }
  return {direction, position};
}, {direction: 1, position: [0,0]});

console.log(Math.abs(part1.position[0]) +  Math.abs(part1.position[1]));

const part2 = input.reduce(({position, waypoint}, [instruction, arg]) => {
  switch (instruction) {
    case 'L':
    case 'R':
      for(let i = 0; i < 4 - arg; i++) waypoint = [waypoint[1], waypoint[0] * -1];
      break;
    case 'N':
    case 'E':
    case 'S':
    case 'W':
      const dir = {N:0,E:1,S:2,W:3}[instruction]
      waypoint[0] += arg * directions[dir][0];
      waypoint[1] += arg * directions[dir][1];
      break;
    case 'F':
      position[0] += arg * waypoint[0];
      position[1] += arg * waypoint[1];
      break;
  }
  return {position, waypoint};
}, {waypoint: [1,10], position: [0,0]})

console.log(Math.abs(part2.position[0]) +  Math.abs(part2.position[1]));
