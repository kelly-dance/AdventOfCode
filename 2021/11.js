import { id, multiLoop, readAdvent, Vec2 } from '../tools.ts';

const inp = (await readAdvent()).split('\n').map(s => s.split('').map(c => parseInt(c)));

let flashes = 0;

for(let i = 0; true; i++){
  let flashed = inp.map(r => r.map(() => false))
  for(const [_, x, y] of multiLoop(2, inp)) {
    const inc = (x, y) => {
      if(inp[x]?.[y] === undefined || flashed[x][y]) return;
      inp[x][y]++;
      if(inp[x][y] === 10) {
        flashed[x][y] = true;
        inp[x][y] = 0;
        flashes++;
        for(const offset of [...Vec2.SIDES, ...Vec2.DIAGONALS]){
          inc(x + offset.x, y + offset.y);
        }
      }
    }
    inc(x, y);
  }
  if(i === 100) console.log(flashes);
  if(flashed.every(r => r.every(id))) {
    console.log(i+1);
    break
  }
}
