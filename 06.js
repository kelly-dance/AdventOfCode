import { readFile, sum, intersection } from './tools.ts';

const input = readFile('inputs/06.txt').split('\r\n\r\n');

console.log(sum(input.map(g => new Set(g.split('').filter(c => /[a-z]/.test(c))).size)))
console.log(sum(input.map(g => intersection(...g.split('\r\n').map(l => new Set(l.split('')))).size)))
