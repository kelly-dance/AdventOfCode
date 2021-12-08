import { countMatches,  numberFromDigits, readAdvent, sum } from '../tools.ts';

const setString = (s: Set<string>) => [...s].sort().join('');

const inp: [Set<string>[], Set<string>[]][] = (await readAdvent()).split('\n').map(s => { // each line
  return s.split(' | ').map(half => { // each half
    return half.split(' ').map(n => new Set(n)) // each word mapped to set of chars
  }) as [Set<string>[], Set<string>[]];
});

// Part 1
console.log(sum(inp.map(l => countMatches(l[1], v => [2,3,4,7].indexOf(v.size) !== -1))));

// Part 2
const resolved = inp.map(([nums, value]) => {
  const mappings = new Map();

  const one = nums.find(s => s.size === 2)!;
  mappings.set(setString(one), 1);

  const four = nums.find(s => s.size === 4)!;
  mappings.set(setString(four), 4);

  const seven = nums.find(s => s.size === 3)!;
  mappings.set(setString(seven), 7);

  const eight = nums.find(s => s.size === 7)!;
  mappings.set(setString(eight), 8);

  const six = nums.find(s => s.size === 6 && ![...one].every(p => s.has(p)))!;
  mappings.set(setString(six) , 6);

  const nine = nums.find(s => s.size === 6 && [...four].every(p => s.has(p)))!;
  mappings.set(setString(nine), 9);

  const zero = nums.find(s => s.size === 6 && s !== six && s !== nine)!;
  mappings.set(setString(zero), 0);

  const five = nums.find(s => s.size === 5 && [...s].every(p => six.has(p)))!;
  mappings.set(setString(five), 5);

  const three = nums.find(s => s !== five && s.size === 5 && [...s].every(p => nine.has(p)))!;
  mappings.set(setString(three), 3);

  const two = nums.find(s => !mappings.has(setString(s)))!;
  mappings.set(setString(two), 2);

  return numberFromDigits(value.map(s => mappings.get([...s].sort().join(''))).reverse());
})

console.log(sum(resolved));
