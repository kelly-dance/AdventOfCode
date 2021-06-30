import { DefaultMap, digitsOfBigInt, bigIntFromDigits } from '../tools.ts';

export type MachineState = {
  running: boolean,
  complete: boolean,
  pointer: bigint,
  relBase: bigint,
  memory: DefaultMap<bigint, bigint>,
}

export type MachineInterface = {
  write(data: bigint): [halt: boolean, callback?: () => any],
  read(): [result: bigint, halt: boolean, callback?: () => any],
}

type ArgInterface = {
  location: bigint,
  mode: bigint,
  readRaw(): bigint,
  read(): bigint,
  write(value: bigint): void,
}

export const terminal: MachineInterface = {
  read: () => {
    const inp = prompt('>');
    if(inp === null) throw new Error('No input!');
    return [BigInt(parseInt(inp)), false];
  },
  write: data => {
    console.log(data.toString())
    return [false];
  }
}

type Operator = (state: MachineState, args: ArgInterface[], mInterface: MachineInterface) => void

const operators: Map<bigint, Operator> = new Map();

// add
operators.set(1n, (state, accessor) => {
  accessor[2].write(accessor[0].read() + accessor[1].read())
  state.pointer += 4n
})

// multiply
operators.set(2n, (state, accessor) => {
  accessor[2].write(accessor[0].read() * accessor[1].read())
  state.pointer += 4n
})

// read
operators.set(3n, (state, accessor, mInterface) => {
  const [data, halt, callback] = mInterface.read();
  accessor[0].write(data);
  state.pointer += 2n
  if(halt) {
    state.running = false;
    if(callback) callback();
  }
})

// write
operators.set(4n, (state, accessor, mInterface) => {
  const data = accessor[0].read();
  const [halt, callback] = mInterface.write(data);
  state.pointer += 2n
  if(halt) {
    state.running = false;
    if(callback) callback();
  }
})

// jump if true
operators.set(5n, (state, accessor) => {
  if(accessor[0].read() !== 0n) state.pointer = accessor[1].read()
  else state.pointer += 3n
})

// jump if false
operators.set(6n, (state, accessor) => {
  if(accessor[0].read() === 0n) state.pointer = accessor[1].read()
  else state.pointer += 3n
})

// less than
operators.set(7n, (state, accessor) => {
  accessor[2].write(accessor[0].read() < accessor[1].read() ? 1n : 0n)
  state.pointer += 4n
})

// equals
operators.set(8n, (state, accessor) => {
  accessor[2].write(accessor[0].read() === accessor[1].read() ? 1n : 0n)
  state.pointer += 4n
})

// set rel base
operators.set(9n, (state, accessor) => {
  state.relBase += accessor[0].read()
  state.pointer += 2n
})

// end
operators.set(99n, (state, accessor) => {
  state.running = false;
  state.complete = true;
})

export const prepareState = (program: number[]): MachineState => {
  const memory = new DefaultMap<bigint, bigint>(() => 0n);
  for(let i = 0; i < program.length; i++){
    memory.set(BigInt(i), BigInt(program[i]));
  }
  return {
    memory,
    pointer: 0n,
    relBase: 0n,
    running: false,
    complete: false,
  }
}

export const run = (state: MachineState, mInterface: MachineInterface) => {
  if(state.complete) throw new Error('Trying to boot a machine that is already complete?');
  if(state.running) throw new Error('Trying to boot a machine that is already running?');
  state.running = true;
  while(state.running){
    const opinfo = state.memory.get(state.pointer);
    const digits = digitsOfBigInt(opinfo, 5);

    const opcode = bigIntFromDigits(digits.slice(0, 2));
    if(!operators.has(opcode)) throw new Error(`Invalid op code! ${opcode}, position: ${state.pointer}`);

    const modes = digits.slice(2);
    const argAccesssors: ArgInterface[] = modes.map((mode, i) => {
      const location = state.pointer + BigInt(i + 1);
      const accessor: ArgInterface = {
        mode,
        location,
        readRaw: () => state.memory.get(location),
        read: () => {
          if(mode === 0n) return state.memory.get(accessor.readRaw());
          if(mode === 1n) return accessor.readRaw();
          if(mode === 2n) return state.memory.get(state.relBase + accessor.readRaw());
          throw new Error('Unsupported parameter mode')
        },
        write: value => {
          if(mode === 0n) state.memory.set(accessor.readRaw(), value);
          else if(mode === 1n) throw new Error('Cannot write in immediate mode');
          else if(mode === 2n) state.memory.set(state.relBase + accessor.readRaw(), value);
          else throw new Error('Unsupported parameter mode')
        }
      }
      return accessor;
    })

    operators.get(opcode)!(state, argAccesssors, mInterface);
  }
  return state;
}

type ScriptMessageProvide = {
  mode: 'provide',
  data: bigint,
  target: number,
}
type ScriptMessageReceive = {
  mode: 'receive',
  target: number,
}
type ScriptMessage = ScriptMessageProvide | ScriptMessageReceive
type Script = (provide: (val: bigint) => ScriptMessageProvide, receive: () => ScriptMessageReceive) => Generator<ScriptMessage, void, bigint>

export const scriptManager = (machine: MachineState, script: Script) => {
  const provide = (val: bigint): ScriptMessageProvide => ({ mode: 'provide', data: val, target: 0 })
  const receive = (): ScriptMessageReceive => ({ mode: 'receive', target: 0 })
  const scriptGen = script(provide, receive);
  let lastMsg: IteratorResult<ScriptMessage> = scriptGen.next();
  const mInterface: MachineInterface = {
    read: () => {
      // console.log(`Read`)
      if(lastMsg.done) throw new Error('Reached end of script early');
      if(lastMsg.value.mode !== 'provide') throw new Error('Tried to read a value when script was not providing');
      const response = lastMsg.value.data
      lastMsg = scriptGen.next();
      return [response, false];
    },
    write: data => {
      // console.log(`Write`)
      if(lastMsg.done) throw new Error('Reached end of script early');
      if(lastMsg.value.mode !== 'receive') throw new Error('Tried to read a value when script was not receiving');
      lastMsg = scriptGen.next(data);
      return [false];
    },
  }
  run(machine, mInterface);
}

type MultiScript = (
  computers: {
    machine: MachineState,
    provide: (val: bigint) => ScriptMessageProvide,
    receive: () => ScriptMessageReceive
  }[]
) => Generator<ScriptMessage, void, bigint>

export const multiScriptManager = (machines: MachineState[], script: MultiScript): void => {
  const scriptGen = script(machines.map((machine, i) => {
    return {
      machine,
      receive: () => ({ mode: 'receive', target: i }),
      provide: val => ({ mode: 'provide', target: i, data: val }),
    }
  }));
  let lastMsgs: (IteratorResult<ScriptMessage> | undefined)[] = machines.map(() => undefined);
  const interfaces: MachineInterface[] = machines.map((_, i) => ({
    read: () => {
      // console.log(`Read ${i}`, lastMsgs[i]?.value)
      if(lastMsgs[i]?.done) throw new Error('Reached end of script early');
      if(lastMsgs[i]?.value.mode !== 'provide') throw new Error('Tried to read a value when script was not providing');
      const response = lastMsgs[i]?.value.data
      const next = scriptGen.next();
      if(next.done) return [response, false];
      const nextIndex = next.value.target;
      lastMsgs[nextIndex] = next;
      if(nextIndex === i) return [response, false];
      else{
        return [response, true, () => {
          run(machines[nextIndex], interfaces[nextIndex])
        }];
      }
    },
    write: data => {
      // console.log(`Write ${i}`, lastMsgs[i]?.value)
      if(lastMsgs[i]?.done) throw new Error('Reached end of script early');
      if(lastMsgs[i]?.value.mode !== 'receive') throw new Error('Tried to read a value when script was not receiving');
      const next = scriptGen.next(data);
      if(next.done) return [false];
      const nextIndex = next.value.target;
      lastMsgs[nextIndex] = next;
      if(nextIndex === i) return [false];
      else{
        return [true, () => {
          run(machines[nextIndex], interfaces[nextIndex])
        }];
      }
    },
  }));
  const first = scriptGen.next();
  if(first.done) throw new Error('Empty script?');
  const firstIndex = first.value.target;
  lastMsgs[firstIndex] = first;
  run(machines[firstIndex], interfaces[firstIndex]);
}
