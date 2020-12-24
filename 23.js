import { readFile, digitsOf, range, numberFromDigits, mod } from './tools.ts';


(() => { // PART 1
  let  nums = digitsOf(readFile('./inputs/23.txt')).reverse();

  const lim = 100;
  
  for(let i = 0; i < lim; i++){
    const [ ...next3 ] = range(3).map(j => nums[j+1]);
    let target;
    for(let j = 1; j < 5; j++){
      target = mod((nums[0]-j-1),9) + 1;
      if(!next3.includes(target)) break;
    }
    let nextNums = []
    for(const n of nums.slice(1)){
      if(next3.includes(n)) continue;
      nextNums.push(n);
      if(n === target) nextNums.push(...next3);
    }
    nextNums.push(nums[0])
    nums = nextNums;
  }
  
  while(nums[0] !== 1){
    nums.unshift(nums.pop());
  }
  
  const fin = nums.slice(1);
  
  console.log(numberFromDigits(fin.slice(0).reverse()))
})();

/**
 * @typedef {{
 *  forward: Node,
 *  num: number,
 * }} Node
 */

(() => { // PART 2
  const inputNums = digitsOf(readFile('./inputs/23.txt')).reverse();
  const numLim = 1_000_000
  const itrLim = 10_000_000;

  /** @type {Node[]} */
  const nodes = new Array(numLim + 1);

  let prev;
  for(let i = 0; i < numLim; i++){
    const num = i < inputNums.length ? inputNums[i] : i + 1;
    const cur = { num, forward: null };
    if(prev) prev.forward = cur;
    prev = cur;
    nodes[num] = cur;
  }

  /** @type {Node} */
  let cur = nodes[inputNums[0]];

  nodes[numLim].forward = cur;

  for(let _ = 0; _ <= itrLim; _++){
    const next3 = cur.forward;
    const target = (() => {
      for(let i = 1; i < 5; i++){
        const attempt = nodes[mod(cur.num - i - 1, numLim) + 1];
        let check = next3;
        let valid = true;
        for(let j = 0; j < 3; j++){
          if(check.num === attempt.num){
            valid = false;
            break;
          }
          check = check.forward;
        }
        if(valid) return attempt;
      }
    })();
    const afterTarget = target.forward;
    cur.forward = next3.forward.forward.forward;
    target.forward = next3;
    next3.forward.forward.forward = afterTarget;
    cur = cur.forward;
  }

  console.log(nodes[1].forward.num * nodes[1].forward.forward.num);
})();
