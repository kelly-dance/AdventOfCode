import * as t from '../tools.ts';

const inp = await t.readAdvent();

const expand = (hex: string) => {
  let out = '';
  for(let i = 0; i < hex.length; i++)
    out += parseInt(hex[i], 16).toString(2).padStart(4, '0');
  return out;
}

type OpPacketId = 0 | 1 | 2 | 3 | 5 | 6 | 7;
type PacketId = 4 | OpPacketId;
type Packet = { version: number } & (
  {
    id: 4,
    val: number
  } | {
    id: OpPacketId,
    val: Packet[]
  }
)
type SpecificPacket<K extends PacketId> = Packet & { id: K };

const parsePacket = (s: string, p: number): [Packet, number] =>  {
  if(p > s.length) throw new Error('Exceeded length of input?');
  const version = parseInt(s.slice(p, p + 3), 2);
  const id = parseInt(s.slice(p + 3, p + 6), 2) as PacketId;
  p += 6;
  if(id === 4){
    let content = '';
    while(true){
      content += s.slice(p + 1, p + 5)
      if(s[p] === '0') {
        p += 5;
        break;
      }
      p += 5;
    }
    return [{version, id, val: parseInt(content, 2)}, p];
  }else{
    const lenslen = s[p] === '1' ? 11 : 15;
    p++;
    const len = parseInt(s.substring(p, p + lenslen), 2);
    let packs: Packet[] = [];
    p += lenslen;
    if(lenslen === 15){
      const initlen = p;
      while(p - initlen < len){
        const [subPacket, newPos] = parsePacket(s, p);
        p = newPos;
        packs.push(subPacket);
      }
    }else{
      for(let i = 0; i < len; i++){
        const [subPacket, newPos] = parsePacket(s, p);
        p = newPos;
        packs.push(subPacket);
      }
    }
    return [{version, id: id, val: packs}, p]
  }
}

const versionSum = (p: Packet): number => {
  if(typeof p.val === 'number') return p.version;
  return p.version + t.sum(p.val.map(versionSum))
}

type PacketEvaluator<T extends PacketId> = (p: SpecificPacket<T>) => number;
interface PacketFnMap extends Map<PacketId, (p: Packet) => number> {
  set<K extends PacketId>(key: K, val: PacketEvaluator<K>): this;
  get<K extends PacketId>(key: K): PacketEvaluator<K>;
}
const packetEvaluators: PacketFnMap = new Map();
packetEvaluators.set(0, p =>  t.sum(p.val.map(evaluate)));
packetEvaluators.set(1, p =>  t.product(p.val.map(evaluate)));
packetEvaluators.set(2, p =>  Math.min(...p.val.map(evaluate)));
packetEvaluators.set(3, p =>  Math.max(...p.val.map(evaluate)));
packetEvaluators.set(4, p =>  p.val);
packetEvaluators.set(5, p =>  evaluate(p.val[0]) > evaluate(p.val[1]) ? 1 : 0);
packetEvaluators.set(6, p =>  evaluate(p.val[0]) < evaluate(p.val[1]) ? 1 : 0);
packetEvaluators.set(7, p =>  evaluate(p.val[0]) === evaluate(p.val[1]) ? 1 : 0);

const evaluate = (p: Packet): number => packetEvaluators.get(p.id)(p);

const packet = parsePacket(expand(inp), 0)[0];
console.log(Deno.inspect(packet,{depth:100,colors:true}))
console.log(versionSum(packet))
console.log(evaluate(packet))
