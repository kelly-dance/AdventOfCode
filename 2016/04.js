import { readFile } from '../tools.ts';

const inp = readFile('./inputs/04.txt').split('\n').map(l => l.substring(0, l.length-1).split('[')).map(([first, checksum]) => {
  const parts = first.split('-');
  const sector = parseInt(parts[parts.length - 1]);
  return {
    name: parts.slice(0, parts.length - 1),
    sector,
    checksum
  }
})

let sum = 0;

for(const room of inp){
  let valid = true;
  const count = {};
  for(const part of room.name){
    for(const char of part){
      if(!(char in count)) count[char] = 0;
      count[char]++;
    }
  }
  const sorted = Object.entries(count).sort((a, b) => {
    if(a[1] > b[1]) return -1
    if(a[1] < b[1]) return 1
    return a[0] < b[0] ? -1 : 1;
  });
  for(let i = 0; i < 5; i++){
    if(sorted[i][0] !== room.checksum[i]) {
      valid = false;
      break;
    }
  }
  if(valid){
    sum += room.sector;
    let name = room.name.map(part => {
      return part.split('').map(c => {
        let code = c.charCodeAt(0) - 97;
        const modded = (code + room.sector) % 26;
        return String.fromCharCode(modded + 97);
      }).join('');
    }).join(' ')
    if(name.includes('north')) console.log(name, room.sector)
  }
}

console.log(sum)
