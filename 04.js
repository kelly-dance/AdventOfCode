import { countMatches, inRange, readFile } from './tools.ts';

const types = {
  byr: s => /^\d{4}$/.test(s) && inRange(Number(s), 1920, 2002, true),
  iyr: s => /^\d{4}$/.test(s) && inRange(Number(s), 2010, 2020, true),
  eyr: s => /^\d{4}$/.test(s) && inRange(Number(s), 2020, 2030, true),
  hgt: s => (
    /\d+cm$/.test(s) && 
    inRange(Number(s.match(/^(\d+)cm$/)[1]), 150, 193, true)
  ) || (
    /\d+in$/.test(s) && 
    inRange(Number(s.match(/^(\d+)in$/)[1]), 59, 76, true)
  ),
  hcl: s => /^#[a-f0-9]{6}$/.test(s),
  ecl: s => 'amb blu brn gry grn hzl oth'.split(' ').includes(s),
  pid: s => /^\d{9}$/.test(s),
}

/** @type {[string, string][][]} */
const input = readFile('./inputs/04.txt')
  .split('\r\n\r\n')
  .map(
    group => group.split(/ |\r\n/).map((field, i, o) => {
      const matches = field.match(/([a-z]+):([a-z0-9#]+)/);
      return [matches[1], matches[2]];
    })
  )

console.log(countMatches(input, fields => Object.keys(types).every((type) => fields.some(([fieldType]) => type === fieldType))));

console.log(
  countMatches(
    input, 
    fields => 
      Object.entries(types).every(
        ([type, check]) => 
          fields.some(
            ([fieldType, val]) => type === fieldType && check(val)
          )
      )
  )
);