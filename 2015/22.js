import { readFile, pickIntsFromString } from '../tools.ts';

const [ initHp, dmg ] = readFile('./inputs/22.txt').split('\n').map(l => pickIntsFromString(l)[0])

/** @typedef {{
 *  hp: number,
 *  mana: number,
 *  used: number,
 *  bossHp: number,
 *  sheild: number,
 *  poison: number,
 *  recharge: number,
 * }} State */

/** @type {(state: State) => void} */
const applyBossTurn = state => {
  applyEffects(state);
  if(state.bossHp > 0) state.hp -= state.sheild ? dmg - 7 : dmg;
}

/** @type {(state: State) => State} */
const cloneState = state => {
  /** @type {State} */
  const clone = JSON.parse(JSON.stringify(state));
  applyEffects(clone);
  return clone;
}

/** @type {(state: State) => void} */
const applyEffects = state => {
  if(state.sheild) state.sheild--;
  if(state.poison) {
    state.poison--;
    state.bossHp -= 3;
  }
  if(state.recharge) {
    state.recharge--;
    state.mana += 101;
  }
}

/** @typedef {{
 *  possible: (state: State) => boolean,
 *  apply: (state: State) => void,
 * }} Move */

/** @type {Move} */
const magicMassile = {
  debug: 'magicMassile',
  possible: state => state.mana > 53,
  apply: state => {
    state.used += 53;
    state.mana -= 53;
    state.bossHp -= 4;
  }
}

/** @type {Move} */
const drain = {
  debug: 'drain',
  possible: state => state.mana > 73,
  apply: state => {
    state.used += 73;
    state.mana -= 73;
    state.bossHp -= 2;
    state.hp += 2;
  }
}

/** @type {Move} */
const sheild = {
  debug: 'sheild',
  possible: state => state.mana > 113 && state.sheild <= 1,
  apply: state => {
    state.used += 113;
    state.mana -= 113;
    state.sheild = 6;
  }
}

/** @type {Move} */
const poison = {
  debug: 'poison',
  possible: state => state.mana > 173 && state.poison <= 1,
  apply: state => {
    state.used += 173;
    state.mana -= 173;
    state.poison = 6;
  }
}

/** @type {Move} */
const recharge = {
  debug: 'recharge',
  possible: state => state.mana > 229 && state.recharge <= 1,
  apply: state => {
    state.used += 229;
    state.mana -= 229;
    state.recharge = 5;
  }
}

const moves = [magicMassile, drain, sheild, poison, recharge];

(()=>{
  const queue = [{
    hp: 50,
    mana: 500,
    used: 0,
    bossHp: initHp,
    sheild: 0,
    poison: 0,
    recharge: 0,
  }];
  
  let bestUsed = Infinity;
  
  while(queue.length){
    const state = queue.pop();
    if(state.used >= bestUsed) continue;
    if(state.bossHp <= 0){
      bestUsed = state.used;
      continue;
    }
    if(state.hp <= 0) continue;
    for(const move of moves){
      if(!move.possible(state)) continue;
      const newState = cloneState(state);
      move.apply(newState);
      applyBossTurn(newState);
      if(newState.hp <= 0) continue;
      queue.push(newState);
    }
  }
  
  console.log(bestUsed);
})();

(()=>{
  const queue = [{
    hp: 50,
    mana: 500,
    used: 0,
    bossHp: initHp,
    sheild: 0,
    poison: 0,
    recharge: 0,
  }];
  
  let bestUsed = Infinity;
  
  while(queue.length){
    const state = queue.pop();
    if(state.used >= bestUsed) continue;
    if(state.bossHp <= 0){
      bestUsed = state.used;
      continue;
    }
    state.hp--;
    if(state.hp <= 0) continue;
    for(const move of moves){
      if(!move.possible(state)) continue;
      const newState = cloneState(state);
      move.apply(newState);
      applyBossTurn(newState);
      if(newState.hp <= 0) continue;
      queue.push(newState);
    }
  }
  
  console.log(bestUsed);
})();
