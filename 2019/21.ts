import { readAdvent, printChar } from '../tools.ts';
import * as int from './intcode.ts';

const prog = (await readAdvent()).split(',').map(s => parseInt(s));

//!(A && B && C) && D
const part1 = `
OR A T
AND B T
AND C T
NOT T J
AND D J
WALK
`.split('\n').filter(Boolean).map(l => `${l}\n`).join('');

// int.scriptManager(int.prepareState(prog), function*(send, rec, type){
//   while((yield type()) === int.ScriptIOMode.receive) printChar(String.fromCharCode(Number(yield rec())))
//   for(let i = 0; i < part1.length; i++){
//     yield send(BigInt(part1.charCodeAt(i)));
//   }
//   while(true) {
//     const val = Number(yield rec());
//     if(val < 256) printChar(String.fromCharCode(val));
//     else console.log(val);
//   }
// })

/*
123456789
ABCDEFGHI

AND(
  OR(
    !B && !E
    !C && !F
    !A
  )
  D
)

AND(
  OR(
    !((B || E) && (C || F))
    !A
  )
  D
)

*/
const part2 = `
OR B T
OR E T
OR C J
OR F J
AND T J
NOT J J
NOT A T
OR T J
AND D J
RUN
`.split('\n').filter(Boolean).map(l => `${l}\n`).join('');

int.scriptManager(int.prepareState(prog), function*(send, rec, type){
  while((yield type()) === int.ScriptIOMode.receive) printChar(String.fromCharCode(Number(yield rec())))
  for(let i = 0; i < part2.length; i++){
    yield send(BigInt(part2.charCodeAt(i)));
  }
  while(true) {
    const val = Number(yield rec());
    if(val < 256) printChar(String.fromCharCode(val));
    else console.log(val);
  }
})
