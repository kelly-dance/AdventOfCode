import { readFile, sum } from './tools.ts';

const add = (a, b) => a + b;
const mult = (a, b) => a * b;

const parseEqn = str => {
  const parseOp = seg => {
    if(seg.charAt(1) === '+') return [add, seg.substring(3)];
    if(seg.charAt(1) === '*') return [mult, seg.substring(3)];
    console.error('invalid op:',seg.substring(0,10))
  }
  const parseNum = seg => {
    if(seg.startsWith('(')){
      const [firstNum, afterFirst] = parseNum(seg.substring(1));
      let nums = [firstNum];
      let ops = [];
      seg = afterFirst;
      while(!seg.startsWith(')')){
        const [op, rest] = parseOp(seg);
        ops.push(op);
        const [num, rest2] = parseNum(rest);
        nums.push(num);
        seg = rest2;
      }
      return [[nums, ops], seg.substring(1)];
    }else{
      const end = seg.split('').findIndex(c => !/\d/.test(c));
      const num = Number(seg.substring(0, end));
      return [num, seg.substring(end)]
    }
  }
  return parseNum('('+str+')')[0];
}

const evalEqnPart1 = eqn => {
  if(typeof eqn === 'number') return eqn;
  if(Array.isArray(eqn)){
    let [raw, ops] = eqn;
    let nums = raw.map(evalEqnPart1);
    return ops.reduce((acc, op, i) => op(acc, nums[i + 1]), nums[0]); // left to right
  }
  throw new Error('invalid eqn');
};

const evalEqnPart2 = eqn => {
  if(typeof eqn === 'number') return eqn;
  if(Array.isArray(eqn)){
    let [raw, ops] = eqn;
    let nums = raw.map(evalEqnPart2);
    [add, mult].forEach(op => { // add left to right then mult left to right
      let numIdx = 0;
      for(let i = 0; i < ops.length; i++){
        if(ops[i] === op){
          nums[numIdx] = op(nums[numIdx], nums[numIdx+1]);
          nums = nums.filter((_, j) => j !== numIdx + 1); // jank way to remove element from array
        }else numIdx++;
      }
      ops = ops.filter(o => o !== op);
    });
    return nums[0];
  }
  throw new Error('invalid eqn');
};

const input = readFile('inputs/18.txt').split('\r\n').map(parseEqn);

console.log(sum(input.map(evalEqnPart1)));
console.log(sum(input.map(evalEqnPart2)));
