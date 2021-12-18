import { memoize, readFile, range, sum } from '../tools.ts';
import * as m from 'https://cdn.skypack.dev/mathjs';

const inp = readFile('./inputs/06.txt').split(',').map(s => parseInt(s));

// original implementation that I know gets the correct answer 
const calc = memoize(remdays => remdays <= 0 ? 1 : calc(remdays - 7) + calc(remdays - 9));

// following along this youtube video
// https://www.youtube.com/watch?v=A5tBvxDM9V4
// it said to do this
// F(n) = r^n => r^n = r^n-7 + r^n-9 => r^9-r^2-1=0
// then we are using newton's method to find the roots
const f = x => m.subtract(m.subtract(m.pow(x, 9), m.pow(x,2)), 1)
// its derivative
const fp = x => m.subtract(m.multiply(m.pow(x, 8), 9), m.multiply(x,2))

// newtons method
const ans = [];
for(let r = -2; r <= 2; r += 0.2){
  for(let i = -2; i <= 2; i += 0.2){
    let guess = m.Complex(r, i);
    for(const _ in range(200)){
      guess = m.subtract(guess, m.divide(f(guess), fp(guess)));
    }
    if(m.abs(guess) > 10) continue; // some points were diverging for some reason idk
    if(!ans.some(a => m.abs(m.subtract(a, guess)) < 0.01)) ans.push(guess);
  }
}

// say roots = r[0], r[1]...
// F(n) = a * r[0]^n + b * r[1]^n + c * r[2]^n ...
// 9 roots, so 9 unknowns (a,b,c...), need 9 equations 
const matrix = range(ans.length).map(p => ans.map(a => m.pow(a, p)));
const targets = range(ans.length).map(i => calc(i));

// find solutions to matrix
const sols = m.lusolve(matrix, targets).map(s => s[0])

// math lib probabl has this built in but idk
const msum = nums => nums.reduce((a,c) => m.add(a,c), 0);

// use the formula like defined on line 32. 
// im just taking the real portion and rounding it so I have a whole number output
const test = x => Math.round(msum(range(ans.length).map(i => m.multiply(sols[i], m.pow(ans[i], x)))).re);

console.log(sum(inp.map(f => test(80 - f))));
console.log(sum(inp.map(f => test(256 - f))));
