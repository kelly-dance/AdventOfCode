import * as t from '../tools.ts';

const inp = t.readFile('./inputs/05.txt');

let pass = '';
let pass2 = t.range(8).map(() => '_');

let i = 0;
while(pass.length < 8 || pass2.includes('_')){
  if(i % 5e5 === 0) console.log(i);
  const hash = t.MD5.strToRaw(`${inp}${i}`)
  const str = t.uint8ToHex(hash.slice(0, 4));
  if(str.startsWith('00000')) {
    if(pass.length < 8) pass += str[5];
    const index = parseInt(str[5]);
    if(!isNaN(index) && index < 8 && pass2[index] === '_'){
      pass2[index] = str[6];
    }
    console.log(i, `${inp}${i}`, str, pass, pass2.join(''))
  }
  i++;
}

console.log(pass)
console.log(pass2.join(''))
