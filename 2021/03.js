import * as t from '../tools.ts';

const inp = (await t.readAdvent()).split('\n')

const counts = t.range(inp[0].length).map(()=>0);

for(const s of inp){
  for(let i = 0; i < s.length; i++){
    if(s[i] === '1') counts[i]++;
  }
}

const gamma = parseInt(counts.map(c => c >= inp.length / 2 ? '1' : '0').join(''), 2);
const epsilon = ~gamma & (2 ** counts.length - 1)

console.log(gamma * epsilon);

let oxyl = inp.slice(0);
for(let b = 0; b < inp[0].length && oxyl.length > 1; b++){
  let oxyCount = t.countMatches(oxyl, e => e[b] === '1');
  oxyl = oxyl.filter(e => e[b] === (oxyCount >= oxyl.length / 2 ? '1' : '0'));
}

let co2l = inp.slice(0);
for(let b = 0; b < inp[0].length && co2l.length > 1; b++){
  let co2Count = t.countMatches(co2l, e => e[b] === '1');
  co2l = co2l.filter(e => e[b] === (co2Count >= co2l.length / 2 ? '0' : '1'));
}

console.log(parseInt(oxyl[0], 2) * parseInt(co2l[0], 2))