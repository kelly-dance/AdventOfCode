import { combos, readFile, sum, takeAll, zip, zipWithIndex } from './tools.ts';

/**
 * @type {({action:'mask',value: string} | {action:'mem',value:number,location:number})[]}
 */
const input = readFile('inputs/14.txt').split('\r\n').map(line => {
  const [left, right] = line.split(' = ');
  if(left === 'mask') return {
    action: 'mask',
    value: right,
  }
  else return {
    action: 'mem',
    location: Number(left.match(/(\d+)/)[0]),
    value: Number(right),
  }
});

(() => { // PART 1
  /** @type {string} */
  let mask = input[0].value;
  /** @type {Map<number,number>} */
  const mem = new Map();
  for(const ins of input){
    switch(ins.action){
      case 'mask':
        mask = ins.value;
        break;
      case 'mem':
        const val = parseInt(zip(ins.value.toString(2).padStart(36, '0').split(''), mask.split('')).map(([b, m]) => m === 'X' ? b : m).join(''), 2);
        mem.set(ins.location, val);
        break;
    }
  }
  console.log(sum(takeAll(mem.values())))
})();


(() => {
  /** @type {string} */
  let mask = input[0].value;
  /** @type {Map<number,number>} */
  const mem = new Map();
  for(const ins of input){
    switch(ins.action){
      case 'mask':
        mask = ins.value;
        break;
      case 'mem':
        /** @type {string[]} */
        const loc = zip(ins.location.toString(2).padStart(36, '0').split(''), mask.split('')).map(([b, m]) => ({0:b,1:'1',X:'X'}[m]));
        const zipped = zipWithIndex(loc).filter(([c]) => c === 'X');
        const count = zipped.length;
        const go = () => mem.set(parseInt(loc.join(''), 2), ins.value);
        if(count > 0){
          for(const combo of combos(['0','1'], count)){
            for(let i = 0; i < count; i++) loc[zipped[i][1]] = combo[i];
            go();
          }
        } else go();
        break;
    }
  }
  console.log(sum(takeAll(mem.values())))
})();
