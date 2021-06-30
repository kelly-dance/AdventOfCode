import { readFile, Vec2, range, maxBy, groupBy } from '../tools.ts';

const input = readFile('./inputs/10.txt').split('\n').map(l => l.split('').map(c => c === '#'))

const prep = cam => {
  const answer = [];
  for(let y = 0; y < input.length; y++){
    for(let x = 0; x < input[y].length; x++){
      if(input[y][x]){
        const loc = new Vec2(x, y);
        if(loc.eq(cam)) continue
        const disp = loc.sub(cam);
        answer.push({
          location: loc,
          disp: disp,
        })
      }
    }
  }
  return answer;
}

const location = maxBy(
  range(input.length)
    .map(y => range(input[y].length).map(x => new Vec2(x, y)))
    .flat(1)
    .filter(v => input[v.y][v.x]),
  cam => new Set(prep(cam).map(o => o.disp.angle())).size
)

console.log(new Set(prep(location).map(o => o.disp.angle())).size)

const groups = groupBy(prep(location), o => o.disp.angle())

const angleMap = a => (a + Math.PI * (2.5)) % (Math.PI * 2);

const angles = [...groups.keys()].sort((a,b) => angleMap(a) - angleMap(b));

for(const key of angles){
  groups.get(key).sort((a, b) => a.disp.len() - b.disp.len());
}

let hits = 0;
while(hits < 200){
  for(const angle of angles){
    const elem = groups.get(angle).shift();
    if(elem) hits++;
    if(hits === 200) {
      console.log(elem.location.x * 100 + elem.location.y)
    }
  }
}

