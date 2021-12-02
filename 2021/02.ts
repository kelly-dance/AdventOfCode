import * as t from '../tools.ts';

const input = (await t.readAdvent()).split('\n').map(l => {
  const [inst, dists] = l.split(' ');
  const dist = parseInt(dists);
  return [inst, dist] as [string, number];
});

(()=>{ // Part 1
  let depth = 0;
  let hor = 0;
  for(const [inst, dist] of input){
    switch(inst){
      case 'up':
        depth -= dist;
        break
      case 'down':
        depth += dist;
        break
      case 'forward':
        hor += dist;
        break
    }
  }
  console.log(depth * hor)
})();

(()=>{ // Part 2
  let depth = 0;
  let hor = 0;
  let aim = 0;
  for(const [inst, dist] of input){
    switch(inst){
      case 'up':
        aim -= dist;
        break
      case 'down':
        aim += dist;
        break
      case 'forward':
        hor += dist;
        depth += aim * dist;
        break
    }
  }
  console.log(depth * hor)
})();


