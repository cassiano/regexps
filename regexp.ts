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
  precededBy,
  error,
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

  const match = map(
    delimitedBy(openParens, precededBy(optional(charSequence('?:')), regExp), closeParens),
    expr => ({
      type: 'parenthesized' as const,
      expr,
    })
  )(input)

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
        const maxCounts = limits.maxCounts as number[] | undefined // 1 dimension (standard array)
        const [i0] = iterationLevelIndices
        maxCount = maxCounts?.[i0]
        break
      }

      default:
        throw new Error(`Unsupported level ${level}`)
    }

    const maxLimit = maxCount ?? limits.max ?? defaultLimits.max

    debug(
      () =>
        `maxCount: ${maxCount}, limits.max: ${limits.max}, defaultLimits.max: ${defaultLimits.max}, maxLimit: ${maxLimit}`
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

const DOLLAR_SIGN = '$'

const endOfString: Parser<string> = input =>
  input.length === 0 ? ['', ''] : [error('Input not empty'), input]

export const evaluateRegExpPart =
  (part: RegExpTypePart): Parser<string> =>
  input => {
    switch (part.type) {
      case 'singleChar': {
        let parser: Parser<string>

        switch (part.character) {
          case PERIOD:
            parser = anyChar()
            break
          case DOLLAR_SIGN:
            parser = endOfString
            break
          default:
            parser = char(part.character)
        }

        const [result, rest] = parser(input)

        debug(() => `Trying to match '${part.character}' against '${input}'`)

        if (!isError(result)) debug(() => `Matched singleChar: '${result}'`)

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
        debug(() => `[repetition] level: ${part.level}, part.limits: ${inspect(part.limits)}`)

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
            const maxCounts = part.limits.maxCounts as number[] // 1 dimension (standard array)
            const [i0] = iterationLevelIndices
            maxCounts[i0] = result.length
            break
          }

          default:
            throw new Error(`Unsupported level ${part.level}`)
        }

        debug(
          () =>
            `Matched repetition (level: ${part.level}, min: ${part.limits.min}, max ${
              part.limits.max
            }): '${result.join(EMPTY_STRING)}', count: ${result.length}`
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

  debug(() => `RegExp: ${replaceEscapedChars(replaceCharacterClassAbbreviations(regExpAsString))}`)

  if (isError(result) || rest !== EMPTY_STRING) throw new Error('Invalid regular expression')

  addIndicesToRepetitions(result)

  return result
}

export const regExpParserFromAST = (ast: RegExpType): Parser<string> =>
  concat(andN(ast.map(evaluateRegExpPart)))

const findRepetitions = (ast: RegExpType): RepetitionType[] => {
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

const addIndicesToRepetitions = (ast: RegExpType, index = 0) => {
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

let levelWaterMark: number

type IterationIndicesType = number | IterationIndicesType[] // Examples: Level 0 -> undefined, Level 1 -> 5, Level 2 -> [3, 4, 6], Level 3 -> [ [2, 4], [1, 3], [2, 5] ] etc.

const pruneFurtherIterationsForRemainingRepetitions = (
  currentRepetition: RepetitionType,
  allRepetitions: RepetitionType[],
  iterationIndices?: IterationIndicesType
) => {
  let i: number, j: number

  levelWaterMark = Math.min(levelWaterMark, currentRepetition.level!)

  switch (currentRepetition.level) {
    case 0: // 0 dimensions (single number)
      i = iterationIndices as number
      break

    case 1: {
      // 1 dimension (standard array)
      ;[i, j] = iterationIndices as [number, number]
      break
    }

    default:
      throw new Error(`Unsupported level ${currentRepetition.level}`)
  }

  allRepetitions.forEach(repetition => {
    if (repetition.limits.maxCounts === undefined) return

    debug(
      () =>
        `Pruning iterations for repetition with level ${repetition.level} and index ${repetition.index}: maxCounts ${repetition.limits.maxCounts}, iteration indices: ${iterationIndices}`
    )

    switch (repetition.level) {
      case 0: // 0 dimensions (single number)
        if (repetition.level! < levelWaterMark || repetition.index! > i)
          repetition.limits.maxCounts = undefined

        break

      // 1 dimension (standard array)
      case 1: {
        const maxCounts = repetition.limits.maxCounts as number[]

        maxCounts.splice(
          repetition.level! < levelWaterMark ? 0 : j + (repetition.index! > i ? 0 : 1)
        )

        break
      }

      default:
        throw new Error(`Unsupported level ${repetition.level}`)
    }
  })
}

export const backtrack = (ast: RegExpType) => {
  const repetitions: RepetitionType[] = findRepetitions(ast)

  const repetitionsByLevel = groupBy(repetitions, repetition => repetition.level!)

  const sortedRepetitionsByLevel = Object.keys(repetitionsByLevel)
    .map(levelAsString => Number(levelAsString))
    .sort()
    .reverse()

  for (const level of sortedRepetitionsByLevel) {
    const sortedLevelRepetitions = repetitionsByLevel[level].sort(
      (left, right) => left.index! - right.index!
    )

    switch (level) {
      case 0: {
        const maxCounts = sortedLevelRepetitions.map(
          // 0 dimensions (single number)
          repetition => repetition.limits.maxCounts as number | undefined
        )

        for (let col = maxCounts.length - 1; col >= 0; col--) {
          const max = maxCounts[col]

          if (max !== undefined && max > sortedLevelRepetitions[col].limits.min) {
            const i = col

            debug(
              () =>
                `[level: ${level}, i: ${i}] Reducing max from ${maxCounts[i]} to ${
                  maxCounts[i]! - 1
                }, maxCounts: ${maxCounts}`
            )

            sortedLevelRepetitions[i].limits.maxCounts = max - 1

            pruneFurtherIterationsForRemainingRepetitions(sortedLevelRepetitions[i], repetitions, i)

            return true
          }
        }

        break
      }

      case 1: {
        const maxCounts = sortedLevelRepetitions.map(
          // 1 dimension (standard array)
          repetition => (repetition.limits.maxCounts ?? []) as (number | undefined)[]
        )

        for (let col = maxCounts[0].length - 1; col >= 0; col--) {
          for (let row = maxCounts.length - 1; row >= 0; row--) {
            const max = maxCounts[row][col]

            if (max !== undefined && max > sortedLevelRepetitions[row].limits.min) {
              const [i, j] = [row, col]

              debug(
                () =>
                  `[level: ${level}, i: ${i}, j: ${j}] Reducing max from ${maxCounts[i][j]} to ${
                    maxCounts[i][j]! - 1
                  }, maxCounts: ${maxCounts[row]}`
              )

              maxCounts[i][j]!--

              pruneFurtherIterationsForRemainingRepetitions(
                sortedLevelRepetitions[i],
                repetitions,
                [i, j]
              )

              return true
            }
          }
        }

        break
      }

      default:
        throw new Error(`Unsupported level ${level}`)
    }
  }

  return false
}

export const buildAndMatch = (
  regExpAsString: string,
  input: string,
  exactMatch = false
): { match: ParserResult<string>; steps: number } => {
  let result, rest
  let steps: number = 0

  if (regExpAsString.startsWith('^')) {
    regExpAsString = regExpAsString.slice(1)
    exactMatch = true
  }

  // Try to match the regular expression from left to right.
  for (let i = 0; i < (exactMatch ? 1 : input.length); i++) {
    let stop: boolean = false

    const ast = buildRegExpAST(regExpAsString)
    const parser = regExpParserFromAST(ast)

    const slicedInput = input.slice(i)

    levelWaterMark = Infinity
    steps = 0
    stop = false

    while (!stop) {
      steps++
      ;[result, rest] = parser(slicedInput)

      if (isError(result)) stop = !backtrack(ast)
      else return { match: [result, rest], steps }
    }
  }

  return { match: [result!, rest!], steps }
}

export const scan = (regExpAsString: string, input: string): string[] => {
  let stop = false
  let rest = input
  const matches = []

  while (!stop) {
    const [result, remaining] = buildAndMatch(regExpAsString, rest).match

    if (!isError(result)) {
      matches.push(result)

      rest = remaining
    }

    if (isError(result) || remaining === EMPTY_STRING) stop = true
  }

  return matches.flat()
}

export const debugRegExp = (regExpAsString: string, input: string): ParserResult<string> => {
  levelWaterMark = Infinity

  let steps = 0
  const ast = buildRegExpAST(regExpAsString)
  const parser = regExpParserFromAST(ast)

  let stop = false
  let match: ParserResult<string> | undefined

  while (!stop) {
    steps++

    match = parser(input)

    if (isError(match[0])) {
      print(ast)

      log(() => `\nlevelWaterMark: ${levelWaterMark}`)

      prompt('\nPress ENTER to continue')

      log('\n-------------------------------------------------------------------\n')

      stop = !backtrack(ast)
    } else {
      stop = true
    }
  }

  print(ast)

  debug(() => `Steps: ${steps}`)

  return match!
}

declare const Deno: {
  inspect: (...args: unknown[]) => void
  stdin: { read: (...args: unknown[]) => void }
}

export const log = console.log

export const inspect = (value: object) => Deno.inspect(value, { depth: 999, colors: true })
export const print = (value: object) => log(inspect(value))

export const showRegExp = (regExpAsString: string) => print(buildRegExpAST(regExpAsString))

export const groupBy = <T>(collection: T[], fn: (prop: T) => string | number) =>
  collection.reduce((acc: { [key: string | number]: T[] }, obj: T) => {
    acc[fn(obj)] ??= []
    acc[fn(obj)].push(obj)
    return acc
  }, {})

export const times = <T>(n: number, fn: (index: number) => T): T[] => [...Array(n).keys()].map(fn)

let DEBUG = true

export const debug = (messageOrFalse: () => string | false): void => {
  if (DEBUG) {
    const message = messageOrFalse()

    if (message === false) return

    log(message)
  }
}

// import * as re from './regexp.ts'; import * as pc from '../reactive-spreadsheet/src/parser_combinators.ts'
//
// re.buildAndMatch('a*', '...aa')
// re.buildAndMatch('a+', '...aa')
//
// re.buildAndMatch('(x+x+)+y', 'xxxxxxxxxx', true) // 558 steps
// re.buildAndMatch('(a+)*ab', 'aaaaaaaaaaaab', true) // 2050 steps
// re.buildAndMatch('.*.*=.*', 'x=x', true) // 6 steps
//
// re.debugRegExp('(x+x+)+y', 'xxxxxxxxxx')
//
// const ast = re.buildRegExpAST('(x+x+)+y')
// const parser = re.regExpParserFromAST(ast)
// re.print(ast)
// parser('xxxxxxxxxx')
