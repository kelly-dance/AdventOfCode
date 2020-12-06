import { readFile, sum } from './tools.ts';

console.log(sum(readFile('inputs/06.txt').split('\r\n\r\n').map(g => [...new Set(g.split('').filter(c => /[a-z]/.test(c)))].length)))
console.log(sum(readFile('inputs/06.txt').split('\r\n\r\n').map(g => g.split('\r\n').reduce((prev, cur) => prev.split('').filter(c => cur.includes(c)).join('')).length)))
