// import {
//   assert,
//   assertEquals,
// } from "https://deno.land/std@0.177.0/testing/asserts.ts";

import {
  Parser,
  and,
  char,
  closeParens,
  comma,
  delimitedBy,
  digit,
  joinedBy,
  letter,
  many1,
  map,
  natural,
  openParens,
  optional,
  or,
  isError,
  or3,
  ParserResult,
  andN,
  charRange,
  orN,
  concat,
  anyChar,
  PERIOD,
  EMPTY_STRING,
  SingleChar,
  allButChar,
  many,
  succeededBy,
  allButCharSet,
  none1,
  ManyNLimitsType,
  or4,
  charSequence,
} from '../reactive-spreadsheet/src/parser_combinators.ts'

type CharacterClassRangeType = { from: SingleChar; to: SingleChar }

type RepetitionLimitsType = {
  min: number
  max: number
}

type MaxCountsType = number | MaxCountsType[] // Examples: Level 0 -> 10, Level 1 -> [3, 4, 6], Level 2 -> [ [2, 4], [1, 3], [2, 5] ] etc.

type SingleCharType = {
  type: 'singleChar'
  character: SingleChar
}
type CharacterClassType = {
  type: 'characterClass'
  negated: boolean
  options: (SingleChar | CharacterClassRangeType)[]
}
type ParenthesizedType = {
  type: 'parenthesized'
  expr: RegExpType
}
type AlternationType = {
  type: 'alternation'
  left: RegExpType
  right: RegExpType
}
type RepetitionType = {
  type: 'repetition'
  expr: SingleCharType | CharacterClassType | ParenthesizedType
  index?: number // Reset for each new level
  level?: number
  limits: RepetitionLimitsType & {
    maxCounts?: MaxCountsType
  }
  maxLimitUpdated?: boolean
}

type RegExpTypePart =
  | SingleCharType
  | CharacterClassType
  | ParenthesizedType
  | RepetitionType
  | AlternationType

type RegExpType = RegExpTypePart[]

const QUANTIFIERS: { [quantifier: SingleChar]: RepetitionLimitsType } = {
  '*': { min: 0, max: Infinity },
  '+': { min: 1, max: Infinity },
  '?': { min: 0, max: 1 },
}

const alternation = char('|')

let currentLevel = 0

const alternativeTerm: Parser<RegExpTypePart> = input =>
  or(
    map(and(succeededBy(many(factor), alternation), many(alternativeTerm)), ([left, right]) => ({
      type: 'alternation' as const,
      left,
      right,
    })),
    factor
  )(input)

const regExp: Parser<RegExpType> = many1(alternativeTerm)

const singleChar: Parser<SingleCharType> = map(
  allButCharSet('|{}[]()' + Object.keys(QUANTIFIERS).join(EMPTY_STRING)),
  character => ({ type: 'singleChar', character } as SingleCharType)
)

const dash = char('-')
const characterClassChar = allButChar(']')

const characterClassOption: Parser<string | CharacterClassRangeType> = or(
  map(or(joinedBy(letter, dash), joinedBy(digit, dash)), range => ({
    from: range[0].toString(),
    to: range[1].toString(),
  })),
  characterClassChar
)

const CARET = '^'

const characterClass: Parser<CharacterClassType> = map(
  delimitedBy(char('['), and(optional(char(CARET)), many1(characterClassOption)), char(']')),
  ([caret, options]) => ({
    type: 'characterClass',
    negated: caret === CARET,
    options,
  })
)

const parenthesized: Parser<ParenthesizedType> = input => {
  currentLevel++

  const match = map(delimitedBy(openParens, regExp, closeParens), expr => ({
    type: 'parenthesized' as const,
    expr,
  }))(input)

  currentLevel--

  return match
}

const quantifier: Parser<RepetitionLimitsType> = map(
  or(
    orN(Object.keys(QUANTIFIERS).map(charSequence)),
    delimitedBy(char('{'), or(joinedBy(optional(natural), comma), natural), char('}'))
  ),
  result =>
    typeof result === 'string'
      ? QUANTIFIERS[result as keyof typeof QUANTIFIERS]
      : typeof result === 'number'
      ? { min: result, max: result }
      : {
          min: result[0] === EMPTY_STRING ? 0 : result[0],
          max: result[1] === EMPTY_STRING ? Infinity : result[1],
        }
)

// const regExpfactor: Parser<RegExpTypePart> = map(
//   and(or3(regExpSingleChar, characterClass, parenthesizedRegExp), optional(quantifier)),
//   ([expr, limits]) =>
//     limits === EMPTY_STRING
//       ? expr
//       : {
//           type: 'repetition',
//           expr,
//           level,
//           limits
//         }
// )

const repetition: Parser<RepetitionType> = map(
  and(or3(singleChar, characterClass, parenthesized), quantifier),
  ([expr, limits]) => ({
    type: 'repetition',
    expr,
    level: currentLevel,
    limits,
  })
)

const factor: Parser<RegExpTypePart> = or4(repetition, singleChar, characterClass, parenthesized)

const iterationLevelIndices: number[] = []

const mutableLimitsManyN =
  <A>(
    parser: Parser<A>,
    limits: ManyNLimitsType & { maxCounts?: MaxCountsType },
    level: number,
    iterationIndex = 0
  ): Parser<A[]> =>
  input => {
    iterationLevelIndices[level] = iterationIndex

    const defaultLimits = { min: 0, max: Infinity }

    let maxCount: number | undefined

    switch (level) {
      case 0: {
        const maxCounts = limits.maxCounts as number | undefined // 0 dimensions (single number)
        maxCount = maxCounts
        break
      }

      case 1: {
        const maxCounts = limits.maxCounts as (number | undefined)[] | undefined // 1 dimension (standard array)
        const [i0] = iterationLevelIndices
        maxCount = maxCounts?.[i0]
        break
      }

      case 2: {
        const maxCounts = limits.maxCounts as (number | undefined)[][] | undefined // 2 dimensions (2D matrix)
        const [i0, i1] = iterationLevelIndices
        maxCount = maxCounts?.[i0]?.[i1]
        break
      }

      case 3: {
        const maxCounts = limits.maxCounts as (number | undefined)[][][] // 3 dimensions
        const [i0, i1, i2] = iterationLevelIndices
        maxCount = maxCounts[i0][i1][i2]
        break
      }

      default:
        throw new Error(`Unsupported level ${level}`)
    }

    const maxLimit = maxCount ?? limits.max ?? defaultLimits.max

    console.log(
      'maxCount:',
      maxCount,
      'limits.max:',
      limits.max,
      'defaultLimits.max',
      defaultLimits.max,
      'maxLimit',
      maxLimit
    )

    if (maxLimit === 0) return [[], input]

    const [result, rest] = parser(input)

    if (isError(result))
      return (limits.min ?? defaultLimits.min) > 0 ? [result, input] : [[], input]

    if (rest.length === input.length) {
      // Successful match but no characters consumed. Avoid an infinite loop.
      return [[], input]
    }

    return map(
      mutableLimitsManyN(
        parser,
        {
          min: (limits.min ?? defaultLimits.min) - 1,
          max: maxLimit - 1,
        },
        level,
        iterationIndex + 1
      ),
      otherResults => [result, ...otherResults]
    )(rest)
  }

export const evaluateRegExpPart =
  (part: RegExpTypePart): Parser<string> =>
  input => {
    switch (part.type) {
      case 'singleChar': {
        const [result, rest] = (part.character === PERIOD ? anyChar() : char(part.character))(input)

        console.log(`Trying to match '${part.character}' against '${input}'`)
        if (!isError(result)) console.log(`%cMatched singleChar: '${result}'`, 'color: green')

        return [result, rest]
      }

      case 'parenthesized':
        return concat(andN(part.expr.map(evaluateRegExpPart)))(input)

      case 'characterClass': {
        const optionsParser = part.options.map(option =>
          typeof option === 'string' ? char(option) : charRange(option.from, option.to)
        )

        return (!part.negated ? orN(optionsParser) : none1(optionsParser))(input)
      }

      case 'repetition': {
        console.log('[repetition] level:', part.level, 'part.limits:', part.limits)

        const [result, rest] = mutableLimitsManyN(
          evaluateRegExpPart(part.expr),
          part.limits,
          part.level!
        )(input)

        if (isError(result)) return [result, input]

        // Clone the object before reassigning to its `maxCounts` property, so distinct iterations
        // do not interfere with one another during backtracking.
        part.limits = { ...part.limits }

        switch (part.level) {
          case 0:
            part.limits.maxCounts = result.length // 0 dimensions (single number).
            break

          case 1: {
            part.limits.maxCounts ??= []
            const maxCounts = part.limits.maxCounts as (number | undefined)[] // 1 dimension (standard array)
            const [i0] = iterationLevelIndices
            maxCounts[i0] = result.length
            break
          }

          case 2: {
            part.limits.maxCounts ??= []
            const maxCounts = part.limits.maxCounts as (number | undefined)[][] // 2 dimensions (2D matrix)
            const [i0, i1] = iterationLevelIndices
            maxCounts[i0] ??= []
            maxCounts[i0][i1] = result.length
            break
          }

          case 3: {
            part.limits.maxCounts ??= []
            const maxCounts = part.limits.maxCounts as (number | undefined)[][][] // 3 dimensions
            const [i0, i1, i2] = iterationLevelIndices
            maxCounts[i0] ??= []
            maxCounts[i0][i1] ??= []
            maxCounts[i0][i1][i2] = result.length
            break
          }

          // case 4: {
          //   part.limits.maxCounts ??= []
          //   const maxCounts = part.limits.maxCounts as (number|undefined)[][][][] // 4 dimensions
          //   const [i0, i1, i2, i3] = iterationLevelIndices
          //   maxCounts[i0] ??= []
          //   maxCounts[i0][i1] ??= []
          //   maxCounts[i0][i1][i2] ??= []
          //   maxCounts[i0][i1][i2][i3] = result.length
          //   break
          // }

          // case 5: {
          //   part.limits.maxCounts ??= []
          //   const maxCounts = part.limits.maxCounts as (number|undefined)[][][][][] // 5 dimensions
          //   const [i0, i1, i2, i3, i4] = iterationLevelIndices
          //   maxCounts[i0] ??= []
          //   maxCounts[i0][i1] ??= []
          //   maxCounts[i0][i1][i2] ??= []
          //   maxCounts[i0][i1][i2][i3] ??= []
          //   maxCounts[i0][i1][i2][i3][i4] = result.length
          //   break
          // }

          // case 6: {
          //   part.limits.maxCounts ??= []
          //   const maxCounts = part.limits.maxCounts as (number|undefined)[][][][][][] // 6 dimensions
          //   const [i0, i1, i2, i3, i4, i5] = iterationLevelIndices
          //   maxCounts[i0] ??= []
          //   maxCounts[i0][i1] ??= []
          //   maxCounts[i0][i1][i2] ??= []
          //   maxCounts[i0][i1][i2][i3] ??= []
          //   maxCounts[i0][i1][i2][i3][i4] = []
          //   maxCounts[i0][i1][i2][i3][i4][i5] = result.length
          //   break
          // }

          default:
            throw new Error(`Unsupported level ${part.level}`)
        }

        console.log(
          `%cMatched repetition (level: ${part.level}, min: ${part.limits.min}, max ${
            part.limits.max
          }): '${result.join(EMPTY_STRING)}', count: ${result.length}`,
          'color: magenta'
        )

        return [result.join(EMPTY_STRING), rest]
      }

      case 'alternation':
        return concat(
          or(andN(part.left.map(evaluateRegExpPart)), andN(part.right.map(evaluateRegExpPart)))
        )(input)

      default: {
        const _exhaustiveCheck: never = part
        throw new Error('Invalid regular expression type')
      }
    }
  }

const CHARACTER_CLASS_ABBREVIATIONS: { [index: SingleChar]: string } = {
  d: '[0-9]', // d = Decimal digit
  b: '[0-1]', // b = Binary digit (non-standard)
  o: '[0-7]', // o = Octal digit (non-standard)
  h: '[0-9a-fA-F]', // h = Hexadecimal digit
  w: '[0-9a-zA-Z_]', // w = Word character
  s: '[ \t\r\n\f]', // s = Space
  r: '[\r\n]', // r = carriage Return
}

const replaceCharacterClassAbbreviations = (regExpAsString: string): string => {
  Object.entries(CHARACTER_CLASS_ABBREVIATIONS).forEach(([abbrev, characterClass]) => {
    // Create regular abbreviations (\d, \h, \w etc, as well as /d, /h, /w etc).
    regExpAsString = regExpAsString
      .replaceAll(`\\${abbrev}`, characterClass)
      .replaceAll(`/${abbrev}`, characterClass)

    // Create "negated" upper-cased abbreviations (\D, \H, \W etc, as well as /D, /H, /W etc).
    abbrev = abbrev.toUpperCase()
    characterClass = characterClass.slice(0, 1) + '^' + characterClass.slice(1)
    regExpAsString = regExpAsString
      .replaceAll(`\\${abbrev}`, characterClass)
      .replaceAll(`/${abbrev}`, characterClass)
  })

  return regExpAsString
}

const replaceEscapedChars = (regExpAsString: string): string =>
  regExpAsString.replaceAll(/[/\\](.)/g, '[$1]')

export const buildRegExpAST = (regExpAsString: string): RegExpType => {
  const [result, rest] = regExp(
    replaceEscapedChars(replaceCharacterClassAbbreviations(regExpAsString))
  )

  console.log('RegExp:', replaceEscapedChars(replaceCharacterClassAbbreviations(regExpAsString)))

  if (isError(result) || rest !== EMPTY_STRING) throw new Error('Invalid regular expression')

  addIndicesToRepetitions(result)

  return result
}

export const regExpParser = (regExpAsString: string): Parser<string> =>
  regExpParserFromAST(buildRegExpAST(regExpAsString))

export const regExpParserFromAST = (ast: RegExpType): Parser<string> =>
  concat(andN(ast.map(evaluateRegExpPart)))

export const matchRegExp = (parser: Parser<string>, input: string): ParserResult<string> => {
  let result!: string | Error
  let rest!: string

  // Try to match the regular expression from left to right.
  for (let i = 0; i < input.length; i++) {
    ;[result, rest] = parser(input.slice(i))

    if (!isError(result)) return [result, rest]
  }

  return [result, rest]
}

export const regExpMatcher =
  (regExpAsString: string): Parser<string> =>
  input => {
    const parser = regExpParser(regExpAsString)

    return matchRegExp(parser, input)
  }

export const scan =
  (regExpAsString: string) =>
  (input: string): string[] => {
    let stop = false
    let rest = input
    const matches = []

    const parser = regExpParser(regExpAsString)

    while (!stop) {
      const [result, remaining] = matchRegExp(parser, rest)

      if (!isError(result)) {
        matches.push(result)

        rest = remaining
      }

      if (isError(result) || remaining === EMPTY_STRING) stop = true
    }

    return matches.flat()
  }

export const findRepetitions = (ast: RegExpType): RepetitionType[] => {
  return ast
    .map(part => {
      switch (part.type) {
        case 'singleChar':
        case 'characterClass':
          return []

        case 'parenthesized':
          return findRepetitions(part.expr)

        case 'repetition':
          return [part].concat(findRepetitions([part.expr]))

        case 'alternation':
          return findRepetitions(part.left).concat(findRepetitions(part.right))

        default: {
          const _exhaustiveCheck: never = part
          throw new Error('Invalid regular expression type')
        }
      }
    })
    .flat()
}

export const addIndicesToRepetitions = (ast: RegExpType, index = 0) => {
  ast.forEach(part => {
    switch (part.type) {
      case 'singleChar':
      case 'characterClass':
        break

      case 'parenthesized':
        addIndicesToRepetitions(part.expr)
        break

      case 'repetition':
        part.index = index++
        addIndicesToRepetitions([part.expr])
        break

      case 'alternation':
        addIndicesToRepetitions(part.left)
        addIndicesToRepetitions(part.right)
        break

      default: {
        const _exhaustiveCheck: never = part
        throw new Error('Invalid regular expression type')
      }
    }
  })
}

export const buildAndMatch = (
  regExpAsString: string,
  input: string
): { steps: number; match: ParserResult<string> } => {
  let stop = false
  let result, rest
  let steps = 0

  const ast = buildRegExpAST(regExpAsString)
  const parser = regExpParserFromAST(ast)

  while (!stop) {
    steps++
    ;[result, rest] = parser(input)

    console.log(`[step #${steps}]`)

    stop = isError(result) ? !backtrack(ast) : true
  }

  return { steps, match: [result!, rest!] }
}

export const sortedRepetitionNodes = (ast: RegExpType) =>
  findRepetitions(ast).sort((left, right) => left.level! - right.level!)

// Find reverse in 1 dimension (standard array).
const findReverse1d = (
  data: (number | undefined)[],
  fn: (value: number | undefined) => boolean
): number | undefined => {
  let i: number = 0

  for (const num of [...data].reverse()) {
    if (fn(num)) return data.length - i - 1

    i++
  }
}

// Find reverse in 2 dimensions (2D matrix).
const findReverse2d = (
  data: (number | undefined)[][],
  fn: (value: number | undefined) => boolean
): [number, number] | undefined => {
  let i = 0

  for (const array1d of [...data].reverse()) {
    const findResult = findReverse1d(array1d, fn)

    if (findResult !== undefined) return [data.length - i - 1, findResult]

    i++
  }
}

// Find reverse in 3 dimensions.
const findReverse3d = (
  data: (number | undefined)[][][],
  fn: (value: number | undefined) => boolean
): [number, number, number] | undefined => {
  let i = 0

  for (const array2d of [...data].reverse()) {
    const findResult = findReverse2d(array2d, fn)

    if (findResult !== undefined) return [data.length - i - 1, ...findResult]

    i++
  }
}

const arrayDifference = <T>(left: T[], right: T[]): T[] =>
  left.filter(item => right.indexOf(item) === -1)

type IterationIndicesType = number | IterationIndicesType[] // Examples: Level 0 -> undefined, Level 1 -> 5, Level 2 -> [3, 4, 6], Level 3 -> [ [2, 4], [1, 3], [2, 5] ] etc.

const pruneFurtherIterationsForRemainingRepetitions = (
  allRepetitions: RepetitionType[],
  processedRepetitions: RepetitionType[],
  iterationIndices?: IterationIndicesType
) => {
  allRepetitions.forEach(repetition => {
    if (repetition.limits.maxCounts === undefined) return

    const wasProcessed = processedRepetitions.indexOf(repetition) >= 0

    console.log(
      `%cPruning iterations for repetition with level ${repetition.level} and index ${repetition.index}: maxCounts ${repetition.limits.maxCounts}, iteration indices: ${iterationIndices}`,
      'color: red'
    )

    switch (repetition.level) {
      case 0: // 0 dimensions (single number)
        repetition.limits.maxCounts = undefined
        break

      // 1 dimension (standard array)
      case 1: {
        const i = iterationIndices as number
        const maxCounts = repetition.limits.maxCounts as (number | undefined)[]

        maxCounts.splice(i + (wasProcessed ? 0 : 1))
        break
      }

      // 2 dimensions (2D matrix)
      case 2: {
        const [i, j] = iterationIndices as [number, number]
        const maxCounts = repetition.limits.maxCounts as (number | undefined)[][]

        maxCounts.splice(i + (wasProcessed ? 0 : 1))
        maxCounts[i].splice(j + (wasProcessed ? 0 : 1))
        break
      }

      // 3 dimensions
      case 3: {
        const [i, j, k] = iterationIndices as [number, number, number]
        const maxCounts = repetition.limits.maxCounts as (number | undefined)[][][]

        maxCounts.splice(i + (wasProcessed ? 0 : 1))
        maxCounts[i].splice(j + (wasProcessed ? 0 : 1))
        maxCounts[i][j].splice(k + (wasProcessed ? 0 : 1))
        break
      }

      default:
        throw new Error(`Unsupported level ${currentLevel}`)
    }
  })
}

export const backtrack = (ast: RegExpType) => {
  const processed: RepetitionType[] = []
  const reversedRepetitions: RepetitionType[] = sortedRepetitionNodes(ast).reverse()

  for (const repetition of reversedRepetitions) {
    if (
      repetition.limits.maxCounts === undefined ||
      (Array.isArray(repetition.limits.maxCounts) && repetition.limits.maxCounts.length === 0)
    )
      continue

    switch (repetition.level) {
      // 0 dimensions (single number)
      case 0: {
        const maxCounts = repetition.limits.maxCounts as number | undefined

        if (maxCounts !== undefined && maxCounts > repetition.limits.min) {
          console.log(
            `%c [level: ${repetition.level}] Reducing max from ${maxCounts} to ${maxCounts - 1}`,
            'color: cyan'
          )

          repetition.limits.maxCounts = maxCounts - 1

          pruneFurtherIterationsForRemainingRepetitions(reversedRepetitions, processed)

          return true
        }

        break
      }

      // 1 dimension (standard array)
      case 1: {
        const maxCounts = repetition.limits.maxCounts as (number | undefined)[]

        const findResult = findReverse1d(
          maxCounts,
          count => count !== undefined && count > repetition.limits.min
        )

        if (findResult !== undefined) {
          const i = findResult

          console.log(
            `%c[level: ${repetition.level}, i: ${i}] Reducing max from ${maxCounts[i]} to ${
              maxCounts[i]! - 1
            }, maxCounts: ${maxCounts}`,
            'color: cyan'
          )

          maxCounts[i]!--

          pruneFurtherIterationsForRemainingRepetitions(reversedRepetitions, processed, i)

          return true
        }

        break
      }

      // 2 dimensions (2D matrix)
      case 2: {
        const maxCounts = repetition.limits.maxCounts as (number | undefined)[][]

        const findResult = findReverse2d(
          maxCounts,
          count => count !== undefined && count > repetition.limits.min
        )

        if (findResult !== undefined) {
          const [i, j] = findResult

          console.log(
            `%c [level: ${repetition.level}, i: ${i}, j: ${j}] Reducing max from ${
              maxCounts[i][j]
            } to ${maxCounts[i][j]! - 1}, maxCounts: ${maxCounts}`,
            'color: cyan'
          )

          maxCounts[i][j]!--

          pruneFurtherIterationsForRemainingRepetitions(reversedRepetitions, processed, [i, j])

          return true
        }

        break
      }

      // 3 dimensions
      case 3: {
        const maxCounts = repetition.limits.maxCounts as (number | undefined)[][][]

        const findResult = findReverse3d(
          maxCounts,
          count => count !== undefined && count > repetition.limits.min
        )

        if (findResult !== undefined) {
          const [i, j, k] = findResult

          console.log(
            `%c [level: ${repetition.level}, i: ${i}, j: ${j}, k: ${k}] Reducing max from ${
              maxCounts[i][j][k]
            } to ${maxCounts[i][j][k]! - 1}, maxCounts: ${maxCounts}`,
            'color: cyan'
          )

          maxCounts[i][j][k]!--

          pruneFurtherIterationsForRemainingRepetitions(reversedRepetitions, processed, [i, j, k])

          return true
        }

        break
      }

      default:
        throw new Error(`Unsupported level ${repetition.level}`)
    }

    processed.push(repetition)
  }

  return false
}

export const debugRegExp = async (
  regExpAsString: string,
  input: string
): Promise<ParserResult<string>> => {
  const bufferSize = 1
  const buf = new Uint8Array(bufferSize)

  const ast = buildRegExpAST(regExpAsString)
  const parser = regExpParserFromAST(ast)

  let stop = false
  let match: ParserResult<string> | undefined

  while (!stop) {
    match = parser(input)

    if (isError(match[0])) {
      print(ast)

      console.log('Press ENTER to continue')
      await Deno.stdin.read(buf)
      console.log('----------------------------\n')

      stop = !backtrack(ast)
    } else {
      stop = true
    }
  }

  return match!
}

declare const Deno: {
  inspect: (...args: unknown[]) => void
  stdin: { read: (...args: unknown[]) => void }
}

export const print = (value: object) =>
  console.log(Deno.inspect(value, { depth: 999, colors: true }))

export const showRegExp = (regExpAsString: string) => print(buildRegExpAST(regExpAsString))

// import * as re from './regexp.ts'; import * as pc from '../reactive-spreadsheet/src/parser_combinators.ts'
//
// re.regExpMatcher('a*')('...aa')
// re.regExpMatcher('a+')('...aa')
//
// re.buildAndMatch('(x+x+)+y', 'xxxxxxxxxx')
//
// await re.debugRegExp('(x+x+)+y', 'xxxxxxxxxx')
//
// const ast = re.buildRegExpAST('(x+x+)+y')
// const parser = re.regExpParserFromAST(ast)
// re.print(ast)
// parser('xxxxxxxxxx')
