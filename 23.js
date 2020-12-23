import { readFile, digitsOf, range, numberFromDigits, mod } from './tools.ts';


(() => { // PART 1
  let  nums = digitsOf(readFile('./inputs/23.txt')).reverse();

  const lim = 100;
  
  for(let i = 0; i < lim; i++){
    // console.log('cups',numberFromDigits(nums.slice(0).reverse()))
    const [ ...next3 ] = range(3).map(j => nums[j+1]);
    // console.log('pickup', next3)
    let target;
    for(let j = 1; j < 5; j++){
      target = mod((nums[0]-j-1),9) + 1;
      if(!next3.includes(target)) break;
    }
    // console.log('destination', target)
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
});//();

/**
 * @typedef {{
 *  forward: Node,
 *  backward: Node,
 *  num: number,
 * }} Node
 */

(() => { // PART 2
  const inputNums = digitsOf(readFile('./inputs/23.txt')).reverse();
  const numLim = 1_000_000
  const itrLim = 10_000_000;

  /** @type {Node[]} */
  const nodes = new Array(numLim+1);

  for(let i = 0; i < inputNums.length; i++){
    const n = inputNums[i];
    nodes[n] = {
      forward: null,
      backward: null,
      num: n,
    }
  }
  for(let i = 0; i < inputNums.length; i++){
    const n = inputNums[i];
    nodes[n].forward = nodes[inputNums[i+1]]
    nodes[n].backward = nodes[inputNums[i-1]]
  }

  for(let i = inputNums.length+1; i <= numLim; i++){
    const cur = {
      forward: null,
      backward: nodes[i-1],
      num: i
    };
    nodes[i] = cur;
    if(i!==inputNums.length+1) nodes[i-1].forward = cur;
  }

  nodes[inputNums.length + 1].backward = nodes[inputNums[inputNums.length - 1]];
  nodes[inputNums[inputNums.length - 1]].forward = nodes[inputNums.length + 1];
  nodes[inputNums[0]].backward = nodes[numLim];
  nodes[numLim].forward = nodes[inputNums[0]];

  /** @type {Node} */
  let cur = nodes[inputNums[0]];

  for(let _ = 0; _ <= itrLim; _++){
    const next3 = cur.forward;
    const target = (() => {
      for(let i = 1; i < 5; i++){
        const attempt = nodes[mod(cur.num-i-1,numLim)+1];
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
    next3.forward.forward.forward.backward = cur;

    target.forward = next3;
    next3.backward = target;
    next3.forward.forward.forward = afterTarget;
    afterTarget.backward = next3.forward.forward;


    cur = cur.forward;
  }

  console.log(nodes[1].forward.num * nodes[1].forward.forward.num);
})();

