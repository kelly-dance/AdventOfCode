export const reservedKeys = '( ) => = ;'.split(' ');

export const tokenizer = (code: string): string[] => {
  const tokens: string[] = [];
  loop: while(code.length){
    code = code.trim();
    for(const key of reservedKeys){
      if(code.startsWith(key)){
        code = code.substring(key.length);
        tokens.push(key)
        continue loop;
      }
    }
    const endOfWord = Math.min(...[' ', ...reservedKeys].map(s => code.indexOf(s)).filter(i => i !== -1));
    if(endOfWord === -Infinity) {
      tokens.push(code);
      break;
    }else{
      const token = code.substring(0, endOfWord);
      code = code.substring(endOfWord);
      tokens.push(token);
    }
  }
  return tokens;
}

type Parser<T> = (tokens: string[], position: number) => ParserResult<T>
type ParserResult<T> = [success: true, result: T, position: number] | [success: false, result: null, position: number];

type FintValue = bigint | string | FintFunct | FintCall;

// these obviously need to be changed, no way to distinguish them at runtime
type FintAssignment = [name: string, value: FintValue]
type FintFunct = [param: string, body: FintValue]
type FintCall = [fn: FintValue, arg: FintValue]

const composeParsers = <T extends any[]>(...parsers: { [K in keyof T]: Parser<T[K]> }): Parser<T> => {
  return (tokens, position) => {
    const vals: any[] = [];
    let pos = position;
    for(const parser of parsers){
      const [success, val, after] = parser(tokens, pos);
      if(!success) return [false, null, position];
      pos = after;
      vals.push(val);
    }
    return [true, vals as T, pos];
  }
}

const anyParser = <T>(...parsers: Parser<T>[]): Parser<T> => {
  return (tokens, pos) => {
    for(const parser of parsers){
      const result = parser(tokens, pos);
      if(result[0]) return result;
    }
    return [false, null, pos];
  }
}

const parseToken = <T extends string>(token: T): Parser<T> => (tokens, pos) => {
  if(tokens[pos] === token) return [true, token, pos + 1];
  return [false, null, pos];
}

const parseFintNumber: Parser<bigint> = (tokens, pos) => {
  if(/^[0-9]+$/.test(tokens[pos])) return [true, BigInt(tokens[pos]), pos + 1];
  return [false, null, pos];
}

const parseFintVariableReference: Parser<string> = (tokens, pos) => {
  const curent = tokens[pos];
  if(!reservedKeys.includes(curent) && /^[a-zA-Z][_a-zA-Z0-9]*$/.test(curent)) return [true, curent, pos + 1];
  return [false, null, pos];
}

const parseFintFnDecl: Parser<FintFunct> = (tokens, pos) => {
  const parts = composeParsers(parseFintVariableReference, parseToken('=>'), parseFintValue)(tokens, pos);
  if(!parts[0]) return [false, null, pos];
  return [true, [parts[1][0], parts[1][2]], parts[2]];
}

const parseParenWrappedVal: Parser<FintValue> = (tokens, pos) => {
  const parts = composeParsers(parseToken('('), parseFintValue, parseToken(')'))(tokens, pos);
  if(!parts[0]) return [false, null, pos];
  return [true, parts[1][1], parts[2]];
}

const parseFintFnCall: Parser<FintCall> = (tokens, pos) => {
  const parts = composeParsers(anyParser(parseFintVariableReference, parseParenWrappedVal), parseParenWrappedVal)(tokens, pos);
  if(!parts[0]) return [false, null, pos];
  return [true, [parts[1][0], parts[1][1]], parts[2]];
}

const parseFintValue: Parser<FintValue> = anyParser<FintValue>(parseFintFnCall, parseFintFnDecl, parseParenWrappedVal, parseFintVariableReference, parseFintNumber);

const parseFile: Parser<FintAssignment[]> = (tokens, position) => {
  const assignments: FintAssignment[] = [];
  let pos = position;
  while(true){
    const part = composeParsers(parseFintVariableReference, parseToken('='), parseFintValue)(tokens, pos);
    if(!part[0]) break;
    const [_, value, newPos] = part;
    pos = newPos;
    const [semi, __, newPos2] = parseToken(';')(tokens, pos);
    if(!semi) throw new Error('Missing semicolon');
    pos = newPos2;
    assignments.push([value[0], value[2]]);
  }
  return [true, assignments, pos];
}

export const parse = (code: string): FintAssignment[] => {
  const result = parseFile(tokenizer(code), 0);
  if(!result[0]) throw new Error('What?')
  return result[1];
}
