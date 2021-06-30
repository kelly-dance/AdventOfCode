import { countMatches, readFile } from '../tools.ts';

const [rulesInput, messagesInput] = readFile('inputs/19.txt').split('\n\n');

const messages = messagesInput.split('\n');

const asArr = rulesInput.split('\n').map((l, i) => {
  const n = Number(l.substring(0, l.indexOf(' ')-1));
  const t = l.substring(l.indexOf(' ')+1)
    .split('|')
    .map(s => {
      return s.split(' ')
        .filter(Boolean)
        .map(s => {
          if(s.startsWith('"')) return s.charAt(1);
          else return Number(s);
        })
    })
  if(t.length === 1 && t[0].length === 1 && typeof t[0][0] === 'string') return [n, t[0][0]];
  return [n,t];
})

/** @type {{[x: number]: string | number[][]}} */
const rules = Object.fromEntries(asArr);

/**
 * @param {string} str 
 * @param {number} ruleIdx 
 * @returns {[true, string[]] | [false]}
 */
const matchRule = (str, ruleIdx) => {
  const rule = rules[ruleIdx];
  if(typeof rule === 'string'){
    if(str.charAt(0) === rule) return [true, [str.substring(1)]];
    return [false];
  }
  const rests = new Set();
  for(const subrule of rule){
    let left = [str];
    for(const subsubrule of subrule){
      const nextLeft = new Set();
      for(const pos of left){
        const [success, possible] = matchRule(pos, subsubrule);
        if(success) possible.forEach(v => nextLeft.add(v));
      }
      left = [...nextLeft];
    }
    left.forEach(v => rests.add(v));
  }
  if(rests.size) return [true, [...rests]];
  return [false];
}

const solve = () => console.log(countMatches(messages, m => {
  const [success, rests] = matchRule(m, 0);
  return success && rests.some(r => r.length === 0);
}));

solve();

rules[8] = [[42],[42,8]];
rules[11] = [[42,31],[42,11,31]];

solve();
