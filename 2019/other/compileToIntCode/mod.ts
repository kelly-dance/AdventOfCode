/*
goal here is to attempt to make a functional programming language that compiles to int code.
I am not really sure yet whats possible.
*/


import { parse } from './parser.ts';
import { readFile } from '../../../tools.ts';


const code = readFile('./samples/playground.fint');

console.log(Deno.inspect(parse(code), {
  depth: 100,
  colors: true,
}))
